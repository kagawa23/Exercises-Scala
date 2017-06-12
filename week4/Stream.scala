// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package week4

import Stream.{empty, _}
import scala.Stream
//import List._

sealed trait Stream[+A] {

  def headOption(): Option[A] =
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

  def tail: Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => t()
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    // Note 1. f can return without forcing the tail
    // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
    // if f requires to go deeply into the stream. So folds sometimes may be
    // less useful than in the strict case
  }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)
    // Note 2. even if f does not force z, foldLeft will continue to recurse
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) => p(h()) || t().exists(p)
    // Note 1. lazy; tail is never forced if satisfying element found this is
    // because || is non-strict
    // Note 2. this is also tail recursive (because of the special semantics
    // of ||)
  }

  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption

  //Exercise 2. Write a function to convert a Stream to a List

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

//  Exercise 3. Write the function take(n) for returning the first n elements of a Stream, and drop(n)
//  for skipping the first n elements of a Stream.

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if (n == 1) => cons(h(), empty)
      case _ => empty
    }
  }

  def drop (n: Int) :Stream[A]= {
    def go(n: Int, s: Stream[A]): Stream[A]={
      if (n==0) s
      else go (n-1, s.tail)
    }
    go(n, this)
  }

//  Exercise 4. Write the function takeWhile (p) for returning all starting elements of a Stream that
//  match the given predicate p.

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons (h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons (h, _) if p(h()) => cons(h(), empty)
    case _ => empty
  }

//  Exercise 5. Implement forAll (p), which checks that all elements in this Stream satisfy a given
//  predicate. Terminate the traversal as soon as it encounters a non-matching value.
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((h,t) => p(h)&&t)
}

//  Exercise 6. Use foldRight to implement takeWhile. Reuse the test case from Exercise 4.5

  def takeWhileViaFold (p: A => Boolean) : Stream[A] = {
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else empty)
  }
//  Exercise 7. Implement headOption using foldRight. Devise a couple of suitable test cases using
//    infinite streams. you can reuse naturals.

  def headOptionViaFold() :Option[A] = foldRight(None: Option[A])((h,_)=> Some(h))

  //Exercise 8. Implement the following functions: map, filter, append, and flatMap using foldRight.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter (f: A=> Boolean): Stream[A] = {
    foldRight(empty[A])((h,t)=> if (f(h)) cons(h,t) else t)
    }

  def append [B>:A] (s: Stream[B]): Stream[B]={
    foldRight(s)((h,t)=> cons(h,t))
  }

  def flatMap[B] (f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])( (h,t) => f(h) append(t))
  }

  //  Exercise 10. Compute a lazy stream of Fibonacci numbers fibs: 0, 1, 1, 2, 3, 5, 8, and so on. It can
  //    be done with functions available so far. Test it be translating to List a finite prefix of fibs, or a
  //    finite prefix of an infinite suffix.

  def fibs(): Stream[Int] = {
    def go(val1: Int, val2: Int): Stream[Int] = {
      cons(val1, go(val1, val1+val2))
    }
    go(0, 1)
  }

  val fib = {
    def fib(val1: Int, val2: Int): Stream[Int] = {
      cons(val1, fib(val1, val1+val2))
    }
    fib(0, 1)
  }

//  Exercise 11. Write a more general stream-building function called unfold. It takes an initial state,
//  and a function for producing both the next state and the next value in the generated stream.
//  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
//  f(z) match {
//    case Some((a, s)) => cons(a, unfold(s)(f))
//    case None => empty
//  }
//}
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq

  //Exercise 1. Define functions from and to that generate streams of natural numbers above (and
//  below) a given natural number.

  def to (n :Int) :Stream[Int] = {
    def go (n: Int, s: Stream[Int]): Stream[Int] ={
      if (n==0) s
      else go(n-1, cons(n, s))
    }
    go (n, empty)
  }

  def from (n :Int) :Stream[Int] = cons(n, from(n+1))
  val naturals : Stream[Int] = from(1)




}

object Tests extends App{
  val s = Stream(1,2,3,4,5,6)
  println(to(5).drop(2).toList)
  println(naturals.take(1000000000).drop(41).take(10).toList)
  println(naturals.takeWhile(_<1000000000).drop(100).take(50).toList)
  println(naturals.headOption())
  println(naturals.headOptionViaFold())
  println(s.filter(_>3).toList)

//  println(naturals.toList)
}

// vim:tw=0:cc=80:nowrap
