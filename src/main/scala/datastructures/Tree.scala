package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /* Exercise 3.25 page 46 */
  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
}