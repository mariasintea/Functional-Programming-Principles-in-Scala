package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit ={
    /*println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

    println("Balanced")
    println(balance("(just an) example".toList))
    println(balance("(just an example".toList))
    println(balance("()(just an) example".toList))
    println(balance(")just an( example".toList))
    print(countChange(4,List(1,2)))*/
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def parantheses(chars: List[Char], leftBrackets: Int, rightBrackets: Int): Boolean = {
      if (chars.isEmpty)
        rightBrackets == leftBrackets
      else
        if (rightBrackets > leftBrackets)
          false
        else
          if (chars.head == '(')
            parantheses(chars.tail, leftBrackets + 1, rightBrackets)
          else
            if (chars.head == ')')
              parantheses(chars.tail, leftBrackets, rightBrackets + 1)
            else
              parantheses(chars.tail, leftBrackets, rightBrackets)
    }
    parantheses(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def findChanges(money: Int, coins: List[Int], solution: List[Int]): Int = {
      if (solution.sum == money)
        1
      else
        if (solution.sum > money)
          0
        else{
          var nr = 0
          for (coin <- coins if solution.length == 0 || coin >= solution.last)
              nr = nr + findChanges(money, coins, solution :+ coin)
          nr
        }
    }

    var sortedCoins = coins.sorted
    findChanges(money, sortedCoins, List[Int]())
  }
