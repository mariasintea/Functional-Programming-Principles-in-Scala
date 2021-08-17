import scala.annotation.tailrec

object MultiplesOf3Or5 extends App{
  def findSumOfMultiples(upperBound: Int):Int = {
    @tailrec
    def loop(x: Int, acc: Int):Int = {
      if (x >= upperBound)
        acc
      else
        if (x % 3 == 0 || x % 5 == 0)
          loop(x + 1, x + acc)
        else
          loop(x + 1, acc)
    }

    loop(1, 0)
  }

  println(findSumOfMultiples(1000))
}
