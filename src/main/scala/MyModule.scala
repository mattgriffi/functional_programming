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

  def findFirst[A](a: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def go(n: Int): Int =
      if (n >= a.length) -1
      else if (p(a(n))) n
      else go(n + 1)
    go(0)
  }

  def isSorted[A](a: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= a.length) true
      else if (!ordered(a(n - 1), a(n))) false
      else go(n + 1)
    go(1)
  }

  private def formatResult(msg: String, x: Int, f: Int => Int): String =
    msg.format(x, f(x))

  private def numberSuffix(x: Int): String =
    if (x % 10 == 1 && x != 11) "st"
    else if (x % 10 == 2) "nd"
    else if (x % 10 == 3) "rd"
    else "th"

  private def formatAbs(x: Int): String = {
    formatResult("The absolute value of %d is %d", x, abs)
  }

  private def formatFactorial(x: Int): String = {
    formatResult("The factorial of %d is %d", x, factorial)
  }

  private def formatFib(x: Int): String = {
    formatResult("The %d" + numberSuffix(x) + " Fibonacci number is %d", x, fib)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatFib(5))
    println(isSorted(Array(1, 2, 6, 5, 5), (x: Int, y: Int) => x <= y))
  }
}
