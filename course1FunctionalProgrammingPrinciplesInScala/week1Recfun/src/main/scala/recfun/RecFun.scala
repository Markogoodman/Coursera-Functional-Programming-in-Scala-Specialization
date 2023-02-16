package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    // println("Pascal's Triangle")
    // for row <- 0 to 10 do
    //   for col <- 0 to row do
    //     print(s"${pascal(col, row)} ")
    //   println()
    println(countChange(4, List(1,2)))

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    // at row n, there is n+1 elements
    if c ==  0 || c == r then 1
    else pascal(c-1, r-1) + pascal(c, r-1)


  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean =
    def helper(position: Int, leftP: Int, rightP: Int): Boolean =
      if (rightP > leftP) {
        return false
      }

      if (position == chars.length) {
        return leftP == rightP
      }
      val c = chars(position)
      var l = leftP
      var r = rightP
      if (c == '(') {
        l += 1
      } else if (c == ')') {
        r += 1
      }
      helper(position+1, l, r)
      
    helper(0, 0, 0)
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    def change(money: Int, p: Int): Int =
      if (p >= coins.length) {
        return 0
      }
      if (money < 0) {
        return 0
      }
      if (money == 0) {
        return 1
      }
      change(money-coins(p), p) + change(money, p+1) 
    
    change(money, 0)


