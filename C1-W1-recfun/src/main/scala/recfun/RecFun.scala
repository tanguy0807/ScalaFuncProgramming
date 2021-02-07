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
    if (c == 0 && r == 0) 1 else
      if (r < 0 | c < 0 | c > r) 0 else
            pascal(c-1, r-1) + pascal(c, r-1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceWithAcc(acc: Int, chars: List[Char]): Boolean = {

      if (chars.isEmpty) {
        if (acc == 0) true
        else false
      } else {
        if (chars.head == '(') balanceWithAcc(acc+1, chars.tail)
        else {
          if (chars.head == ')') {
            if (acc-1 < 0) false
            else balanceWithAcc(acc-1, chars.tail)
          }
          else balanceWithAcc(acc, chars.tail)
        }
      }
    }

    balanceWithAcc(0, chars)
  }



  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else {
      if (money < 0) 0 else
        if (money == 0) 1 else
          if (coins.tail.isEmpty) countChange(money - coins.head, coins)
          else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
