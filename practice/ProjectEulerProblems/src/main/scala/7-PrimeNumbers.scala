import scala.annotation.tailrec

object PrimeNumbers extends App {
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

  def findPrimeByRank(rank: Long): Long = {
    @tailrec
    def loop(counter: Long, currentNumber: Long): Long = {
      if (isPrime(currentNumber)) {
        if (counter + 1 == rank)
          currentNumber
        else
          loop(counter + 1, currentNumber + 2)
      }
      else
        loop(counter, currentNumber + 2)
    }

    if (rank == 1)
      2
    else
      loop(1, 3)
  }

  println(findPrimeByRank(10001))
}