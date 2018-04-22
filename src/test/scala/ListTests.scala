import org.scalatest.{BeforeAndAfter, FunSuite}

class ListTests extends FunSuite with BeforeAndAfter {
  
  var list1: List[Int] = Nil
  var list2: List[Int] = Nil
  var list3: List[Int] = Nil
  var list4: List[Int] = Nil
  var list5: List[Int] = Nil

  before {
    list1 = List(1)
    list2 = List(1, 2)
    list3 = List(1, 2, 3)
    list4 = List(1, 2, 3, 4)
    list5 = List(1, 2, 3, 4, 5)
  }

  test("List.tail") {
    val list = List(2, 3, 4, 5)
    assert(List.tail(list5) === list)
    assert(List.tail(Nil) === Nil)
    assert(List.tail(List(1)) === Nil)
    assert(List.tail(List.tail(list5)) === List.tail(list))
  }

  test("List.setHead") {
    val list = List(-1, 2, 3, 4, 5)
    assert(List.setHead(list5, -1) === list)
    assert(List.setHead(Nil, 5) === List(5))
  }

  test("List.drop") {
    assert(List.drop(Nil, 2) === Nil)
    assert(List.drop(list5, 50) === Nil)
    assert(List.drop(list5, 0) === list5)
    assert(List.drop(list5, 1) === List(2, 3, 4, 5))
    assert(List.drop(list5, 5) === Nil)
    assert(List.drop(list5, 3) === List(4, 5))
    assert(List.drop(list5, -2) === list5)
  }

  test("List.dropWhile") {
    assert(List.dropWhile(list5)(a => a < 3) === List(3, 4, 5))
    assert(List.dropWhile(Nil)((a: Int) => a < 3) === Nil)
    assert(List.dropWhile(list5)(a => a > 0) === Nil)
    assert(List.dropWhile(list5)(a => a < 5) === List(5))
    assert(List.dropWhile(list5)(a => a < 2) === List(2, 3, 4, 5))
    assert(List.dropWhile(list5)(a => a < 1) === list5)
  }

  test("List.init") {
    assert(List.init(Nil) === Nil)
    assert(List.init(list1) === Nil)
    assert(List.init(list2) === list1)
    assert(List.init(list5) === list4)
  }

  test("List.length") {
    assert(List.length(list5) === 5)
    assert(List.length(list4) === 4)
    assert(List.length(list3) === 3)
    assert(List.length(list2) === 2)
    assert(List.length(list1) === 1)
    assert(List.length(Nil) === 0)
  }
}
