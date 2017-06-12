package week2

/**
  *
  * Created by apreda on 09.02.2017.
  */
object Quiz extends App {
  /**
    * This type of of exercises will be in the exam
    * Print all even numbers between 2 * n and 0, in decreasing order
    * @param n a nubmer for printing even integers  between 2 * n and 0, in decreasing order
    * @return the given number
    */
  def f(n: Int): Int = {

    def go(n: Int): Int = {
      if (n % 2 == 0) {
        println(n)
      }
      if (n > 0) {
        go(n - 1)
      } else {
        n
      }
    }
    go(2 * n)
    n
  }
}

object QuizTest {
  def main(args: Array[String]): Unit = {
    assert(Quiz.f(30) == 30)

  }
}

class Printable {
  def hello() {
    println("printable")
  }
}

class Square extends Printable {
  override def hello() {
    println("square")
  }
}

class Triangle extends Printable {
  override def hello() {
    println("triangle")
  }
}

object PrintableTest extends App {
  val x = new Square()
  val y = new Triangle()
  x.hello()
  //in Java all methods are virtual, there is a dynamic dispatch
  x.asInstanceOf[Printable].hello()
  //triangle square
  y.hello()
  y.asInstanceOf[Printable].hello()
}
