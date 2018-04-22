sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /* Exercise 3.2 page 35 */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  /* Exercise 3.3 page 36 */
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => List(a)
    case Cons(_, t) => Cons(a, t)
  }

  /* Exercise 3.4 page 36 */
  @annotation.tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) =>
      if (n > 0) drop(t, n - 1)
      else as
  }

  /* Exercise 3.5 page 36 */
  @annotation.tailrec
  def dropWhile[A](as: List[A], p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) =>
      if (p(h)) dropWhile(t, p)
      else as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }
}
