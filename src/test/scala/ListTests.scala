import org.scalatest.FunSuite

class ListTests extends FunSuite {

  test("List.tail") {
    val list1 = List(1, 2, 3, 4, 5)
    val list2 = List(2, 3, 4, 5)
    assert(List.tail(list1) === list2)
    assert(List.tail(Nil) === Nil)
    assert(List.tail(List(1)) === Nil)
    assert(List.tail(List.tail(list1)) === List.tail(list2))
  }

  test("List.setHead") {
    val list1 = List(1, 2, 3, 4, 5)
    val list2 = List(-1, 2, 3, 4, 5)
    assert(List.setHead(list1, -1) === list2)
    assert(List.setHead(Nil, 5) === List(5))
  }

  test("List.drop") {
    val list1 = List(1, 2, 3, 4, 5)
    assert(List.drop(Nil, 2) === Nil)
    assert(List.drop(list1, 50) === Nil)
    assert(List.drop(list1, 0) === list1)
    assert(List.drop(list1, 1) === List(2, 3, 4, 5))
    assert(List.drop(list1, 5) === Nil)
    assert(List.drop(list1, 3) === List(4, 5))
    assert(List.drop(list1, -2) === list1)
  }

  test("List.dropWhile") {
    val list1 = List(1, 2, 3, 4, 5)
    assert(List.dropWhile(list1, (a: Int) => a < 3) === List(3, 4, 5))
    assert(List.dropWhile(Nil, (a: Int) => a < 3) === Nil)
    assert(List.dropWhile(list1, (a: Int) => a > 0) === Nil)
    assert(List.dropWhile(list1, (a: Int) => a < 5) === List(5))
    assert(List.dropWhile(list1, (a: Int) => a < 2) === List(2, 3, 4, 5))
    assert(List.dropWhile(list1, (a: Int) => a < 1) === list1)
  }

  test("List.init") {
    val list1 = List(1, 2, 3, 4, 5)
    val list2 = List(1)
    val list3 = List(1, 2)
    assert(List.init(Nil) === Nil)
    assert(List.init(list2) === Nil)
    assert(List.init(list3) === List(1))
    assert(List.init(list1) === List(1, 2, 3, 4))
  }
}
