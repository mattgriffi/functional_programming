object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, p: Int, c: Int): Int =
      if (i >= n) c
      else go(i + 1, c, c + p)
    go(1, 0, 1)
  }

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(x, factorial(x))
  }

  private def formatFib(x: Int): String = {
    val msg = "The %d%s Fibonacci number is %d"
    val end =
      if (x % 10 == 1 && x != 11) "st"
      else if (x % 10 == 2) "nd"
      else if (x % 10 == 3) "rd"
      else "th"
    msg.format(x, end, fib(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatFib(5))
  }
}
