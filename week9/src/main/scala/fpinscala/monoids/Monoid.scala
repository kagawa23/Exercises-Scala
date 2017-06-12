// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package week9.src.main.scala.fpinscala.monoids

import scala.language.higherKinds

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  // I guess such objects could be useful if we combine them with implicits

  def listMonoid[A] = new Monoid[List[A]] {
    def op (a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Exercise 10.1

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1+a2
    def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1*a2
    def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1||a2
    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1&&a2
    def zero: Boolean = true
  }

  // Exercise 10.2

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = if (a1.isDefined) a1 else a2
    def zero: Option[A] = None
  }

  def dual[A] (m :Monoid[A]) = new Monoid[A] {
    def op (a1: A, a2: A) = m.op(a2,a1)
    val zero = m.zero
  }

  // Exercise 10.3
  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = (x) => a1(a2(x))
    val zero = (a: A) => a
  }

  // Exercise 10.4 is solved in MonoidSpec.scala
  //
  // I am using scalacheck to test my monoids (not our own framework)
  // run the tests using "sbt test"
  // Look into the file to see how to write property tests in scalacheck.
  // Use that file as an example template for your own tests

  def concatenate[A] (as: List[A], m: Monoid[A]): A =
    as.foldLeft (m.zero) (m.op)

  // Exercise 10.5 (easy)

  def foldMap[A,B] (as: List[A], m: Monoid[B]) (f: A=>B): B = as match {
    case x :: xs => m.op(f(x), foldMap(xs, m) (f))
    case _ => m.zero
  }

  // Exercise 10.7

  def foldMapV[A,B] (v: IndexedSeq[A], m: Monoid[B]) (f: A => B) :B = v.length match {
    case 1 => m.op(f(v.head), m.zero)
    case length if length > 0 => {
      val splitted = v.splitAt(length/2)
      m.op(foldMapV(splitted._1, m) (f), foldMapV(splitted._2, m) (f))
    }
    case _ => m.zero
  }

  // Exercise 9
  //
  // Implement a productMonoid that builds a monoid out of two monoids. Test it
  // with scala check for instance by composing an Option[Int] monoid with a
  // List[String] monoid and running through our monoid laws.

  // def productMonoid[A,B] (ma: Monoid[A]) (mb: Monoid[B]) =

  // Exercise 10.17

  // def functionMonoid[A,B] (mb: Monoid[B]) =

}


trait Foldable[F[_]] {

  def foldRight[A,B] (as: F[A]) (z: B) (f: (A,B) => B): B
  def foldLeft[A,B] (as: F[A]) (z: B) (f: (B,A) => B): B
  def foldMap[A,B] (as: F[A]) (f: A => B) (mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 10.15 (we use the mixed nature of traits here)

  def toList[A] (fa: F[A]) :List[A] = this.foldRight(fa) (Nil:List[A]) ((head, acc) => head::acc)
}

// Exercise 10.12 We just do Foldable[List]

object Foldable extends Foldable[List] {

  def foldRight[A,B] (as: List[A]) (b: B) (f: (A,B) => B): B = as match {
    case Nil => b
    case a::as1 => f(a, foldRight (as1) (b) (f))
  }

  def foldLeft[A,B] (as: List[A]) (b: B) (f: (B,A) => B): B = as match {
    case Nil => b
    case a::as1 => foldLeft (as1) (f(b,a)) (f)
  }

  // using foldLeft would not be a good idea for Streams (there I would use
  // foldRight)
  def foldMap[A,B] (as: List[A]) (f: A => B) (mb: Monoid[B]): B =
  foldLeft (as) (mb.zero) ((b,a) => mb.op(b,f(a)))

}

// Exercise 10.14

object FoldableOption extends Foldable[Option] {

  def foldRight[A,B] (a: Option[A]) (b: B) (f: (A,B) => B): B = a match {
    case None => b
    case Some (a) => f(a,b)
  }

  def foldLeft[A,B] (a: Option[A]) (b: B) (f: (B,A) => B): B = a match {
    case None => b
    case Some (a) => f(b,a)
  }

  def foldMap[A,B] (a: Option[A]) (f: A => B) (mb: Monoid[B]): B =
    foldLeft (a) (mb.zero) ((b,a) => mb.op(b,f(a)))
}