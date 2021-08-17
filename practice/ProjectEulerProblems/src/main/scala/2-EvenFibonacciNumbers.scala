import scala.annotation.tailrec

object EvenFibonacciNumbers extends App{
  def findSum(upperBound: Int): Int ={
    @tailrec
    def loop(a: Int, b: Int, acc: Int): Int ={
      if (a >= upperBound)
        acc
      else
        if (a % 2 == 0)
          loop(b, a + b, acc + a)
        else
          loop(b, a+b, acc)
    }

    loop(1, 2, 0)
  }

  println(findSum(4000000))
}
