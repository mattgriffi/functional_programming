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
}