import scala.annotation.tailrec

object LargestPalindromeProduct extends App{
  def findLargestPalindromeProduct: Int = {
    @tailrec
    def loopReversed(n: Int, acc: Int): Int = {
      if (n < 10)
        n + acc * 10
      else
        loopReversed(n / 10, n % 10 + acc * 10)
    }

    def isPalindrome(n: Int): Boolean = (n == loopReversed(n, 0))

    @tailrec
    def loopFindProducts(elem1: Int, elem2: Int, max: Int): Int = {
      if (elem1 > 999)
          max
      else
        if (elem2 > 999)
            loopFindProducts(elem1 + 1, 100, max)
        else
          if (elem1 * elem2 > max && isPalindrome(elem1 * elem2))
            loopFindProducts(elem1, elem2 + 1, elem1 * elem2)
          else
            loopFindProducts(elem1, elem2 + 1, max)
    }

    loopFindProducts(100, 100, 0)
  }

  println(findLargestPalindromeProduct)
}
