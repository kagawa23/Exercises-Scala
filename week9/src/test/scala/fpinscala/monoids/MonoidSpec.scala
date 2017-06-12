// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
// Example solution for scala exercises using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide
package week9.src.test.scala.fpinscala.monoids

import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import week9.src.main.scala.fpinscala.monoids.Monoid

object MonoidSpec extends Properties("Monoids..") {

  import Monoid._

  // Exercise 4 (intro to the exercise)

  def associative[A :Arbitrary] (m: Monoid[A]) :Prop =
    forAll { (a1: A, a2: A, a3: A) =>
      m.op(m.op(a1,a2), a3) == m.op(a1,m.op(a2,a3)) } :| "associativity"

  def unit[A :Arbitrary] (m :Monoid[A]) =
    forAll { (a :A) => m.op(a, m.zero) == a } :| "right unit" &&
    forAll { (a :A) => m.op(m.zero, a) == a } :| "left unit"

  def monoid[A :Arbitrary] (m :Monoid[A]) :Prop = associative (m) && unit (m)

  property ("stringMonoid is a monoid") = monoid (stringMonoid)

  // Exercise 4: test intAddition, intMultiplication, booleanOr,
  // booleanAnd and optionMonoid.

   property ("intAddition is monoid") = monoid(intAddition)
   property ("intMultiplication is monoid") = monoid (intMultiplication)
   property ("booleanOr is monoid") = monoid(booleanOr)
   property ("booleanAnd is monoid") = monoid(booleanAnd)
   property ("optionMonoid is monoid") = monoid(optionMonoid)
  // property ...

  // Exercise 5
  //Def. A function f from one Monoid A to another Monoid B
  // is a homomorphism if it distributes: (for any x, y) B.op(f(x), f(y)) = f(A.op(x,y))

   def homomorphism[A :Arbitrary,B :Arbitrary]
    (ma: Monoid[A]) (f: A => B) (mb: Monoid[B]) =
     forAll { (x: A, y: A) => mb.op(f(x), f(y)) == f(ma.op(x,y)) }

   //def isomorphism[A :Arbitrary, B :Arbitrary] todo

  property ("stringMonoid and listMonoid[Char] are homomorphic") =
    homomorphism(stringMonoid)(m => m.toList)(listMonoid)

//   property ("stringMonoid and listMonoid[Char] are isomorphic") =

  // Exercise 6 todo

   //property ("booleanOr and booleanAnd are isomorphic") = isomorphism(monoid(booleanAnd))()

  // Exercise 7 (the testing part)

   //property ("productMonoid is a monoid") = monoid(productMonoid(intAddition)(intMultiplication))
}
