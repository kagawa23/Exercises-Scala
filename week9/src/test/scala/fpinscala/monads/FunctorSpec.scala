// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen

// Example solutions for Monad exercises, using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package week9.src.test.scala.fpinscala.monads

import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import week9.src.main.scala.fpinscala.monads.Functor

import scala.language.higherKinds
import week9.src.main.scala.fpinscala.monads.Functor._


object FunctorSpec extends Properties("Functor[F[_]] properties..") {

  def mapLaw[A,F[_]] (fn :Functor[F]) (implicit arb: Arbitrary[F[A]]) :Prop =
    forAll { (fa :F[A]) => fn.map[A,A] (fa) (x=>x) == fa }

  property ("Functor[List[Int]] satisfies the functor law") =
    mapLaw[Int,List](ListFunctor)

  // Exercise 11 (for OptionFunctor)

  property ("Funtor[Option[String]] satisfies the functor law")=
    mapLaw[String, Option](OptionFunctor)
}


// vim:cc=80
