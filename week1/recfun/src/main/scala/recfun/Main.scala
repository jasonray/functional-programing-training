package recfun

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0) 1
    else if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val leftparens: Char = "(".head
    val rightparens: Char = ")".head

    def checkBalanceLoop(remainingChars: List[Char], unbalancedParens: Int): Boolean = {
      if (remainingChars.isEmpty) unbalancedParens == 0
      else if (remainingChars.head == leftparens) checkBalanceLoop(remainingChars.tail, unbalancedParens + 1)
      else if ((remainingChars.head == rightparens) && (unbalancedParens==0)) false
      else if (remainingChars.head == rightparens) checkBalanceLoop(remainingChars.tail, unbalancedParens - 1)
      else checkBalanceLoop(remainingChars.tail, unbalancedParens)
    }

    checkBalanceLoop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 0
}
