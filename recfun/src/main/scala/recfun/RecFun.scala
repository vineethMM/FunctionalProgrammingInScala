package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r - c == 0)
      1
    else
      pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceIn(cs: List[Char], stack: List[Char]): List[Char] = {
       (cs.headOption, stack.headOption) match {
          case (None, _)                => stack
          case (Some('('), _)           => balanceIn(cs.tail, '(' :: stack)
          case (Some(')'), Some('('))   => balanceIn(cs.tail, stack.tail)
          case (Some(')'), _)           => balanceIn(cs.tail, ')' :: stack)
          case _                        => balanceIn(cs.tail, stack)
       }
    }

    balanceIn(chars, List()).isEmpty
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIn(moneyIn: Int, coinsIn: List[Int]): Int = {
       (moneyIn, coinsIn)  match {
          case (0, _)          => 1
          case (m, _) if m < 0 => 0
          case (_, Nil)        => 0
          case (m, c)          => countChangeIn(m - c.head, c) + countChangeIn(m, c.tail)
       }
    }

    if(money == 0) 0 else countChangeIn(money, coins)
  }
}
