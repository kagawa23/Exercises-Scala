package week8.src.main.scala.fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
//import stream01._ // uncomment to test the broken headOption implementation
//import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecWasowski extends FlatSpec with Checkers {

  import Stream._

  // --------------------------------------------------------------------------

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
  for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)

  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
      ("random" |:
        Prop.forAll { (s :Stream[Int]) => s.headOption != None } )

  }

  // This returns the head of an unlimited stream, and since we don't crash
  // with a memory exception, we must have asserted that the tail is not run
  it should "return the head of the stream, and make sure execution stops (03)" in {
    def testStream:Stream[Int] = Stream.cons(0, testStream)
    assert(testStream.headOption == Some(0))
  }

  // --------------------------------------------------------------------------

  behavior of "take"

  it should "not force any heads nor any tails of the Stream it manipulates & not force (n+1)st head (01+02)" in check {
    Prop.forAll { (n :Int) => {
      def testStream:Stream[Int] = cons(throw new AssertionError, throw new AssertionError)
      testStream.take(n)
      true
    }}
  }

  //We put 20 as a hardcoded number inorder to avoid OutOfMemory Exceptions.
  it should "hold that s.take(n).take(n) == s.take(n) for any Stream s and any n (idempotency)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("empty" |:
      Prop.forAll { (n :Int) => empty.take(n).take(n) == empty.take(n) } ) &&
      ("singleton" |:
        Prop.forAll { (n :Int) => cons (n,empty).take(n).take(n).headOption == cons(n,empty).take(n).headOption } ) &&
      ("random" |:
        Prop.forAll {(s :Stream[Int]) => s.take(20).take(20).toList.zip(s.take(20).toList).forall{ case(x,y) => x == y }})
  }

  // --------------------------------------------------------------------------

  behavior of "drop"


  //We put 20 as a hardcoded number inorder to avoid OutOfMemory Exceptions.
  //We have determined that this law will only hold if neither n nor m are negative numbers.
  it should "hold that s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll { (n :Int, m: Int, s:Stream[Int]) => {
      if (n >= 0 && m >= 0)
        s.drop(n).drop(m).take(20).toList.zip(s.drop(n+m).take(20).toList).forall{ case(x,y) => x == y }
      else
        true
    }}
  }

  //We apply modulo 100 to prevent negative or too large inputs to drop, we expect that drop must never evaluate head
  //Our test is expected to fail if a call to drop is not lazy.
  it should "hold that s.drop(n) does not force any of the dropped elements heads" in check {
    Prop.forAll { (n :Int) => {
      def testStream:Stream[Int] = cons(throw new AssertionError(), testStream)
      testStream.drop(n%100)
      true
    }}
  }

  // --------------------------------------------------------------------------

  behavior of "map"

  //We used 100 as a limit for take inorder to avoid out of memory when doing the comparison
  it should "hold that x.map(id) == x" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll { (s:Stream[Int]) => s.map(x=>x).take(100).toList.zip(s.take(100).toList).forall{ case(x,y) => x == y }}
  }

  //Our test will crash on infinit stream
  it should "hold that map terminates on infinite streams" in {
    def teststream:Stream[Int] = cons(0, teststream)
    teststream.map(x => x%2)
  }

  // --------------------------------------------------------------------------

  behavior of "append"

  it should "not force tail or head on the stream that is appended to" in {
    def infinite:Stream[Int] = cons(throw new AssertionError, throw new AssertionError )
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll {
      (s1:Stream[Int]) => {infinite.append(s1); true}
    }
  }

  it should "not force tail or head on the stream that is appended" in {
    def infinite:Stream[Int] = cons(throw new AssertionError, throw new AssertionError )
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll {
      (s1:Stream[Int]) => {s1.append(infinite); true}
    }
  }

  it should "should terminate on any finite stream" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll {
      (s1:Stream[Int], s2:Stream[Int]) => {s1.append(s2); true }
    }
  }

  it should "should terminate on infinite streams" in {
    def teststream:Stream[Int] = cons(0, teststream)
    teststream.append(teststream)
  }

  //We used 50 and 100 as a limit for take inorder to avoid out of memory when doing the comparison
  it should "hold that the appended stream is appended at the end" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll {
      (s1:Stream[Int], s2:Stream[Int]) =>
        s1.take(50).append(s2.take(50)).drop(s1.take(50).toList.length).toList.zip(s2.take(50).toList)
          .forall{ case(x,y) => x == y }
    }
  }

}