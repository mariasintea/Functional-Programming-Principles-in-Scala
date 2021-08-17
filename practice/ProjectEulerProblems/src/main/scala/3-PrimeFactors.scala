import scala.annotation.tailrec

object PrimeFactors extends App{
  def findLargestPrimeFactor(n: Long):Long = {
    @tailrec
    def loopDivision(n: Long, d: Long): Long = {
      if (n % d != 0)
        n
      else
        loopDivision(n/d, d)
    }

    @tailrec
    def loopFindFactors(n: Long, d: Long, max: Long): Long = {
      if (d * d > n) {
        if (n == 1)
          max
        else
          n
      }
      else {
        if (n % d == 0)
          loopFindFactors(loopDivision(n, d), d + 2, d)
        else
          loopFindFactors(n, d + 2, max)
      }
    }

    if (n % 2 == 0)
      loopFindFactors(loopDivision(n, 2), 3, 2)
    else
      loopFindFactors(n, 3, 1)
  }

  println(findLargestPrimeFactor(600851475143L))
}
