import org.scalatest.{BeforeAndAfter, FunSuite}
import datastructures.{Tree, Branch, Leaf}

class TreeTests extends FunSuite with BeforeAndAfter{

  var root: Tree[Int] = Leaf(0)

  before {
    root = Branch(
      Branch(
        Branch(
          Branch(
            Leaf(1),
            Leaf(3)),
          Branch(
            Leaf(10),
            Leaf(5))),
        Leaf(7)),
      Leaf(8))
  }

  test("Tree.size") {
    assert(Tree.size(root) === 11)
  }

  test("Tree.maximum") {
    assert(Tree.maximum(root) === 10)
  }

  test("Tree.depth") {
    assert(Tree.depth(root) === 4)
  }

  test("Tree.map") {
    val plus1 = Branch(
      Branch(
        Branch(
          Branch(
            Leaf(2),
            Leaf(4)),
          Branch(
            Leaf(11),
            Leaf(6))),
        Leaf(8)),
      Leaf(9))
    val string = Branch(
      Branch(
        Branch(
          Branch(
            Leaf("1"),
            Leaf("3")),
          Branch(
            Leaf("10"),
            Leaf("5"))),
        Leaf("7")),
      Leaf("8"))
    assert(Tree.map(root)(_ + 1) === plus1)
    assert(Tree.map(root)(_.toString) === string)
  }
}
