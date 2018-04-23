package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /* Exercise 3.25 page 46 */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /* Exercise 3.26 page 46 */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(r, l) => maximum(l) max maximum(r)
  }

  /* Exercise 3.27 page 46 */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  /* Exercise 3.28 page 46 */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /* Exercise 3.29 page 47 */
  def fold[A, B](t: Tree[A])(z: A => B)(f: (B, B) => B): B = t match {
    case Leaf(n) => z(n)
    case Branch(l, r) => f(fold(l)(z)(f), fold(r)(z)(f))
  }

  /* Exercise 3.29 page 47 */
  def size2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((l, r) => 1 + l + r)
  }

  /* Exercise 3.29 page 47 */
  def maximum2(t: Tree[Int]): Int = {
    fold(t)(x => x)((l, r) => l max r)
  }

  /* Exercise 3.29 page 47 */
  def depth2[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((l, r) => 1 + (l max r))
  }

  /* Exercise 3.29 page 47 */
  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))
  }
}
