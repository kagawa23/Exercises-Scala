package week1

// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  def square (n: Int) :Int = abs(n) * abs(n)

  private def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs (x)} and its square is ${square(x)}"

  val magic :Int = 42
  var result :Option[Int] = None

  def main(args: Array[String]): Unit = {
    assert (magic - 84 == magic.-(84))
    println (formatAbs (magic-100))
  }
}
