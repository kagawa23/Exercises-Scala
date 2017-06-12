/* ADVANCED PROGRAMMING. Monadic Evaluators. Andrzej Wąsowski */
package week10.src.main.scala.adpro.monads

import scala.language.higherKinds

// Work through this file top down

// Section 2.1 [Wadler]

// Wadler uses a langauge similar to Haskell to implement his evaluator. We will
// use scala.  This is Wadler's Term language implemented in Scala:

trait Term
case class Cons (value :Int) extends Term
case class Div (left :Term, right :Term) extends Term


// From now on we create one module (object) per section to avoid name clashes
// between different variants.

// This is the basic evaluator (compare to the paper, to see whether you
// understand)

object BasicEvaluator {

  def eval (term :Term) :Int = term match {
    case Cons(a) => a
    case Div(t,u) => eval (t) / eval (u)
  }

}

// Uncomment sections below as you proceed

// // Section 2.2 [Wadler] Variation one: Exceptions

 object ExceptionEvaluator {

   // an implementation of Wadler's types in Scala
   type Exception = String
   trait M[+A]
   case class Raise (e: String) extends M[Nothing]
   case class Return[A] (a: A) extends M[A]

   // an implementation of direct exception evaluator in Scala:
   //  complete in place of "..."

   def eval (term :Term) :M[Int] = term match {
     case Cons(a) => Return(a)
     case Div(t, u) => eval(t) match {
       case Raise(e) => Raise(e)
       case Return(a) => eval(u) match {
         case Raise(e) => Raise(e)
         case Return(b) => if (b == 0) Raise("devide by zero") else Return(a / b)
       }
     }
   }
   // Once you are done reflect how massive was the change from the
   // BasicEvaluator to the exception evaluator (no need to write anything).
 }

// // Section 2.3 [Wadler] Variation two: State

 object StateEvaluator {

   type State = Int
   case class M[+A] (step: State => (A,State))

   // complete the implementation of the evaluator as per the spec in the
   // paper.

   def eval (term :Term) :M[Int] = term match {
     case Cons (a) => M[Int] (x => (a,x))
     case Div (t,u) => M[Int] (x => {
       val (a, y) = eval(t).step(x)
       val (b, z) = eval(u).step(y)
       (a/b, z+1)
     })
   }

 }

 // Section 2.4 [Wadler] Variation three: Output

 object OutputEvaluator {

   type Output = String
   case class M[+A] (o: Output, a: A)

   def line (a :Term) (v :Int) :Output =
     "eval(" + a.toString + ") <= " + v.toString + "\n"

   def eval (term :Term) :M[Int] = term match {
  case Cons (a) => M(line(Cons(a))(a),a)
  case Div (t,u) => {
    val M(x, a) = eval(t)
    val M(y, b) = eval(u)
    M(x + y + line(Div(t,u))(a/b), a/b)
  }
}
 }

 // Section 2.5 [Wadler] A monadic evaluator

 // The following are two generic monadic interfaces (one for classes, one for
 // meta-classes/objects) that we will use to type check our monadic solutions.
 //
 // We shall provide flatMap and map for our monads to be able to use for
 // comprehensions in Scala.
 //
 // IMPORTANT: flatMap is called "(*)" in the paper.

 trait Monad[+A,M[_]] {
   def flatMap[B] (k: A => M[B]) :M[B]
   def map[B] (k: A => B) :M[B]
 }

 // we will provide unit, as the paper does. This will be placed in a companion
 // object.

 trait MonadOps[M[_]] { def unit [A] (a :A) :M[A] }

 // The above abstract traits will be used to constraint types of all our monadic
 // implementations, just to ensure better type safety and uniform interfaces.



 // Now we are startin to implement the monadic evaluator from the paper.
 // Compare this implementation to the paper, and make sure that you understand
 // the Scala rendering.

 // Section 2.6 [Wadler] Variation zero, revisited: The basic evaluator

 object BasicEvaluatorWithMonads {

   // We enrich our M type with flatMap and map;
   // A flatMap is already in the paper (called *)
   // I add map, so that we can use for comprehensions with this type
   case class M[+A] (a: A) extends Monad[A,M] {
     def flatMap[B] (k: A => M[B]) :M[B] = k (this.a)
     def map[B] (k: A => B) :M[B] = M.unit (k (this.a))
   }

   // The paper also uses unit, so we put it in the companion object
   object M extends MonadOps[M] { def unit[A] (a : A) :M[A] = M[A] (a) }

   def eval (term: Term) :M[Int] = term match {
     case Cons (a) => M.unit (a)
     case Div (t,u) => for {
       a <- eval (t)
       b <- eval (u)
       r <- M.unit (a/b)
     } yield r
   }
   // Make sure that you understand the above implementation (an dhow it
   // relates to the one in the paper). If you find the for comprehension to be
   // obscuring things, you may want to rewrite the above using just map and
   // flatMap.
 }

 // Section 2.7 [Wadler] The monadic evaluator with exceptions

 object ExceptionEvaluatorWithMonads {

   type Exception = String

   trait M[+A] extends Monad[A,M]{

     def flatMap[B] (k: A => M[B]) :M[B] = this match {
        case Raise (e) => Raise (e)
        case Return (a) => k(a)
     }

     def map[B] (k: A => B) :M[B] = this match {
       case Raise (e) => Raise (e)
       case Return (a) => Return (k(a))
     }
   }

   object M extends MonadOps[M] { def unit[A] (a : A) :M[A] = Return (a) }

   case class Raise (e: String) extends M[Nothing]
   case class Return[A] (a: A) extends M[A]

   // complete the evaluator
   def eval (term :Term) :M[Int] = term match {
     case Cons (a) => M.unit (a)
     case Div (t,u) => for {
       a <- eval(t)
       b <- eval(u)
       r <- if (b==0) Raise( "Divided by zero") else M.unit(a/b)
     } yield r
   }


   //  Discuss in the group how the monadic evaluator with exceptions
   // differs from the monadic basic one
 }

 // Section 2.8 [Wadler] Variation two, revisited: State

 object StateEvaluatorWithMonads {

   type State = Int

   case class M[+A] (step: State => (A,State)) extends Monad[A,M] {

     // flatMap is bind or (*) in the paper
     def flatMap[B] (k :A => M[B]) = M[B] {
       x => { val (a,y) = this.step (x); k(a).step(y) } }

     def map[B] (k :A => B) :M[B] =
       M[B] { x => { val (a,y) = this.step(x); (k(a),y) } }
   }

   // complete the implementation of unit, based on the paper
   object M extends MonadOps[M] { def unit[A] (a : A) :M[A] =  M(x => (a,x)) }

   //  complete the implementation of the evalutor:
   def eval (term :Term) :M[State] = term match {
     case Cons (a) => M.unit (a)
     case Div (t,u) => for{
       a <- eval(t)
       b <- eval(u)
       r <- M[State](z => (a / b, z + 1))
     }  yield r
     }

   // Discuss in the group how the monadic evaluator with counter differs
   // from the monadic basic one (or the one with exceptions)

 }

 // Section 2.9 [Wadler] Output evaluator

 object OutputEvaluatorWithMonads {

   type Output = String

   case class M[+A] (o: Output, a: A) {

     // flatMap is (*) in [Wadler]
     // implement flatMap
     def flatMap[B] (k :A => M[B]) =
        {
          val (x, a) = (this.o, this.a)
       val (y,b) = (k(this.a).o, k(this.a).a)
        M (x+y, b)}


     def map[B] (k :A => B) :M[B] = M[B] (this.o, k(this.a))

   }

   // implement unit
   object M { def unit[A] (a : A) :M[A] = M[A]("",a) }

   def line (a :Term) (v :Int) :Output =
     "eval(" + a.toString + ") <= " + v.toString + "\n"

   // TODO: implement eval
//   def eval (term :Term) :M[Int] = term match {
//     case Cons(a) => M.unit(a)
//     case Div(t,u) => for {
//       a <- eval(t)
//       b <- eval(u)
//       r<- M[Output](a+b+line(term)(a/b), a/b)
//     } yield r
//
//   }

   // Discuss in the group how the monadic evaluator with output differs from
   // the monadic basic one (or the one with state/counter).
 }

