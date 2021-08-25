import scala.annotation.tailrec

object SpecialPythagoreanTriplet extends App{
  def findPythagoreanTriplet(N: Int): Int = {
    // a + b + c = N => c = N - a -b
    // a^2 + b^2 = c^2 => a^2 + b^2 = (N - a - b)^2 <=> a^2 + b^2 = N^2 + a^2 + b^2 - 2Na - 2Nb + 2ab
    // => 2Nb - 2ab = N^2 - 2Na => b(2N - 2a) = N^2 - 2Na => b = (N^2 - 2Na)/(2N - 2a)

    @tailrec
    def loop(a: Int, prod: Int): Int = {
      if (a == N)
        prod
      else
        {
          val b = (N * N - 2 * N * a) / (2 * N - 2 * a)
          val c = N - a - b
          if (b > 0 && c > 0 && a * a + b * b == c * c && a < b && b < c)
            loop(N, a * b * c)
          else
            loop(a + 1, prod)
        }
    }

    loop(1, 1)
  }

  println(findPythagoreanTriplet(12))
  println(findPythagoreanTriplet(1000))
}
