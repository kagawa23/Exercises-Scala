package week4

/**
  * Created by apreda on 23.02.2017.
  */
class Quiz extends App {


  trait Function[-T1, +R] extends AnyRef

  //  Function[Nothing, Any] <: Function[Any,Nothing];
  //the above violates the - principle Nothing is not a super type of Any


  //  Function[Any, Nothing] <: Function[Nothing,Any];
  // the first principle is fine (-), and also the second (+)

  // we promise to return Nothing, but return Any, because the + expects a subtype of ANothing, but we get a super type

  //  Function[Any, Any] <: Function[Nothing,Any];
  // it's just like the first one

  //  Function[Nothing, Nothing] <: Function[Any,Any];

  // exercise on variance
  // UML class diagram
  // from last lecture: All the classes extend Nothing?]
  // T1 tells you that this is for single argument function
  // in the definition we use covariance for the first parameter and variance for the second parameter
  // the function will return R
  // this function uses Liskov's principle
  // the minus tells us that we can use any supertype of T1

  lazy val cache = 0

  //from the book
  sealed trait Stream[+A]

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }

    //    def take(n: Int): Stream[A] = //returns a stream that returns all the n elements
    //this is capitalized


    //head and tail above, this are the constructors
    //to avoid using lambda expressions for calling the constructor, we define a convinient constructor (factory)
    //you can use this in practice : ones.take(10).toList
    //todo: figure out these down
    //    def cons[A](hd
    //
    //    :=> A, tl :=> Stream[A]): Stream[A] = {
    //      lazy val HEAD = hd;
    //      lazy val TAIL = tl;
    //      CONS(() => hd, () => tl)
//          def headOption[A] = this match {
//            case empty => None
//            case Cons(h, y) => Some(h()) //head is forced, tail has never been computer/created
//          }
  }

}

//EXAMPLE: val ones is a Stream[Int] = cons(1, ONES)//is an infinite list of 1s; it takes constant time to compute;
//the ones is a cons h : () => 1 and t: ()=> references the cons
