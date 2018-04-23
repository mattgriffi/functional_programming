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
  def dropWhile[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) =>
      if (p(h)) dropWhile(t)(p)
      else as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /* Exercise 3.6 page 37 */
  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(is: List[Int]): Int = {
    foldRight(is, 0)(_ + _)
  }

  def product2(ds: List[Double]): Double = {
    foldRight(ds, 1.0)(_ * _)
  }

  /* Exercise 3.9 page 40 */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  /* Exercise 3.10 page 40 */
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /* Exercise 3.11 page 41 */
  def sum3(is: List[Int]): Int = {
    foldLeft(is, 0)(_ + _)
  }

  /* Exercise 3.11 page 41 */
  def product3(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)(_ * _)
  }

  /* Exercise 3.11 page 41 */
  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  /* Exercise 3.12 page 41 */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))
  }

  /* Exercise 3.13 page 41 */
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  /* Exercise 3.14 page 41 */
  def append2[A](a1: List[A])(a2: List[A]): List[A] =  {
    foldRight2(a1, a2)((a, b) => Cons(a, b))
  }

  /* Exercise 3.15 page 41 */
  def concat[A](as: List[List[A]]): List[A] = {
    foldRight2(as, Nil: List[A])(append2(_)(_))
  }

  /* Exercise 3.16 page 42 */
  def add1(is: List[Int]): List[Int] = {
    map(is)(_ + 1)
  }

  /* Exercise 3.17 page 42 */
  def doubleToString(ds: List[Double]): List[String] = {
    map(ds)(_.toString)
  }

  /* Exercise 3.18 page 42 */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight2(as, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  /* Exercise 3.19 page 42 */
  def filter[A](as: List[A])(p: A => Boolean): List[A] = {
    foldRight2(as, Nil: List[A])((h, t) => if (p(h)) Cons(h, t) else t)
  }

  /* Exercise 3.20 page 42 */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }
}
