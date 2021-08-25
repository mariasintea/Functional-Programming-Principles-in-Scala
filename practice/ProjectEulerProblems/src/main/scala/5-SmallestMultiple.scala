import scala.annotation.tailrec

object SmallestMultiple extends App{
  def findSmallestCommonMultiple(upperBound: Long): Long = {

    // returns the Greatest Common Divisor for two numbers a and b
    @tailrec
    def gcd(a: Long, b: Long): Long = {
      if (b == 0)
        a
      else
        gcd(b, a % b)
    }

    // returns the Smallest Common Multiple for two numbers a and b
    def scm(a: Long, b: Long): Long = {
      (a * b) / gcd(a, b)
    }

    @tailrec
    def loop(x: Long, acc: Long): Long = {
      if (x > upperBound)
        acc
      else
        loop(x + 1, scm(x, acc))
    }

    loop(1L, 1L)
  }

  println(findSmallestCommonMultiple(20L))
}
