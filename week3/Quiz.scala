package week3

/**
  * Created by apreda on 16.02.2017.
  */

/**
  * Write an ADT(in Scala) for representing binary number of arbitrary size. A binary is either zero, or one, or it
  * is a digit (one, zero) followed by a shorter binary number.
  *
  *
  */
//0 is represented by Zero
//1 is represented by One
//101 is represented by I(O(One))
sealed trait BinaryNumber

case object Zero extends BinaryNumber
case object One extends BinaryNumber
case class O(t: BinaryNumber) extends BinaryNumber
case class I(t: BinaryNumber) extends BinaryNumber


//0 -> O2(Empty)
//1->I2(Empty
//101->I(O(I(Empty))))
sealed trait BinaryNumber2 {}

case object Empty extends BinaryNumber2
case class O2(t: BinaryNumber2) extends BinaryNumber2
case class I2(t: BinaryNumber2) extends BinaryNumber2


//todo: make tests testing takes place here
class Quiz extends App {
//    assert(BinaryN)
}
