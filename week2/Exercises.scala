package week2

// Advanced Programming 2017,
// A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2

   def tail[A] (as: List[A]) :List[A] = as match {
     case Nil => Nil
     case Cons(h, t) => t
   }

  // Exercise 3

   def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
     case Nil => Cons (newHead, Nil)
     case Cons(h, t) => Cons (newHead, t)
   }

  // Exercise 4

   def drop[A] (l: List[A], n: Int) : List[A] = l match {
     case Nil => Nil
     case Cons(h,t) => if
       (n > 0) drop(tail(l), n-1)
      else l
   }

  // Exercise 5

   def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
     case Nil => Nil
     case Cons(h,t) => if (f(h)) dropWhile(tail(l), f)
     else l
   }

  // Exercise 6

  def size[A](l: List[A]): Int = {
    def go(l: List[A], size:Int): Int= {
      l match {
        case Nil => size
        case Cons(h,t) => go(tail(l), size+1)
      }
    }
    go (l, 0)
  }

   def init[A](l: List[A]): List[A] = l match {
     case Nil => Nil
     case Cons(h,Nil) => Nil
     case Cons(h,t)=> Cons(h, init(tail(l)))
   }

  // Exercise 7

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

   def length[A] (as: List[A]): Int = foldRight(as, 0)((a: A, l) => l + 1)

  // Exercise 8

//  foldLeft (List(1,2,3,4),0) (_ + _) computes (((0 + 1) + 2) + 3) + 4 while
//  foldRight (List(1,2,3,4),0) (_ + _) computes 1 + (2 + (3 + (4 + 0))).

  @annotation.tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x)) (f)
  }

  // Exercise 9

   def sum (as : List[Int]) : Int = foldLeft(as, 0)(_+_)
   def product (as :List[Int]) : Int = foldLeft(as, 1) (_*_)
   def length1 (as :List[Int]) : Int = foldLeft(as, 0) ((l, a) => l+1)

  // Exercise 10

   def reverse[A] (as :List[A]) :List[A] = foldLeft(as, List[A]())((b, a) => Cons(a,b))

  // Exercise 11

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = {
    foldLeft(reverse(as), z) ((b, a) => f(a, b))
  }

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = {
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
  }

  // Exercise 12

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

   def concat[A] (as: List[List[A]]) :List[A] = foldLeft(as, List[A]())((a, b) => append(a,b))

  // Exercise 13

  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = {
    foldRight(as, Nil:List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)
  }

  // Exercise 14


  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  // Exercise 15

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = {
    flatMap(l)(a => if (p(a)) List(a) else Nil)
  }
  // Exercise 16

   def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
     case (Nil, _) => Nil
     case (_, Nil) => Nil
     case (Cons(h,t), Cons(x,y)) => Cons(h+x, add(t)(y))

   }

  // Exercise 17

   def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = (l,r) match {
     case (Nil, _) => Nil
     case (_, Nil) => Nil
     case (Cons(h,t), Cons(x,y)) => Cons(f(h,x), zipWith(f)(t,y))
   }

  // Exercise 18

  def hasPrefixTA[A](lst: List[A], pref: List[A]): Boolean = (lst, pref) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if x == y => hasPrefixTA(xs, ys)
    case _ => false
  }

  def hasSubsequenceTA[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case _ if hasPrefixTA(sup, sub) => true
      case Cons(hd, tl) => hasSubsequenceTA(tl, sub)
    }
  // Exercise 19

  def pascalTA(n: Int): List[Int] = n match {
    case 0 => Nil
    case 1 => List(1)
    case _ => {
      val p = pascalTA(n - 1)
      append(Cons(1, zipWith[Int, Int, Int](_ + _)(p, tail(p))), List(1))
    }
  }
  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))

  def main(args: Array[String]): Unit = {
    val as = List (1,2,3,4,5,6)
    val as2= List(7,8)
    val as3 =List(9)
    val testTail = tail(as)
    println(testTail)

    println(setHead(as, 99))

    println(drop(as, 2))

    println(dropWhile(as, (a: Int)=> a <4))

    println(size(as))
    println(init(as))

    println(length(as))
    println(length(Nil))

    println(sum(as))
    println(product(as))
    println(length1(as))

    println(reverse(as))

    println(concat(List(as,as2,as3)))

    println(filter(as)(_ > 3))

    println(add(as)(as2))

    println(zipWith((a: Int, b: Int)=> a*b)(as,as2))

  }

}



