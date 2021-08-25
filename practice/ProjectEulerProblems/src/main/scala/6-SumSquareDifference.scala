import scala.annotation.tailrec

object SumSquareDifference extends App{
  def findSumSquareDifference(upperBound: Long): Long = {

    @tailrec
    def sumOfSquares(x: Long, acc: Long): Long = {
      if (x > upperBound)
        acc
      else
        sumOfSquares(x + 1, x * x + acc)
    }

    @tailrec
    def squaredSum(x: Long, acc: Long): Long = {
      if (x > upperBound)
        acc * acc
      else
        squaredSum(x + 1, x + acc)
    }

    squaredSum(1, 0) - sumOfSquares(1, 0)
  }

  println(findSumSquareDifference(100))
}
