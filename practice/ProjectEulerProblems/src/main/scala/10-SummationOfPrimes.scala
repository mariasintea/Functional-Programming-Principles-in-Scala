import scala.annotation.tailrec

object SummationOfPrimes extends App{
  def isPrime(n: Long): Boolean = {
    @tailrec
    def loop(d: Long): Boolean = {
      if (d * d > n)
        true
      else if (n % d == 0)
        false
      else
        loop(d + 2)
    }

    if (n < 2 || n > 2 && n % 2 == 0)
      false
    else
      loop(3)
  }

  def findSummation(upperBound: Int): Long = {
    @tailrec
    def loop(currentNumber: Long, sum: Long): Long = {
      if (currentNumber > upperBound)
        sum
      else
        if (isPrime(currentNumber))
          loop(currentNumber + 1, sum + currentNumber)
        else
          loop(currentNumber + 1, sum)
    }

    loop(3, 2)
  }

  println(findSummation(10))
  println(findSummation(2000000))
}