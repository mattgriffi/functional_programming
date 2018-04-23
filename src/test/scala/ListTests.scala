import org.scalatest.{BeforeAndAfter, FunSuite}
import datastructures.{List, Nil}

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

  test("List.length2") {
    assert(List.length2(list5) === 5)
    assert(List.length2(list4) === 4)
    assert(List.length2(list3) === 3)
    assert(List.length2(list2) === 2)
    assert(List.length2(list1) === 1)
    assert(List.length2(Nil) === 0)
  }

  test("List.sum3") {
    assert(List.sum3(list5) === 1 + 2 + 3 + 4 + 5)
    assert(List.sum3(list4) === 1 + 2 + 3 + 4)
    assert(List.sum3(list3) === 1 + 2 + 3)
    assert(List.sum3(list2) === 1 + 2)
    assert(List.sum3(list1) === 1)
    assert(List.sum3(Nil) === 0)
  }

  test("List.reverse") {
    assert(List.reverse(list5) === List(5, 4, 3, 2, 1))
    assert(List.reverse(list4) === List(4, 3, 2, 1))
    assert(List.reverse(list3) === List(3, 2, 1))
    assert(List.reverse(list2) === List(2, 1))
    assert(List.reverse(list1) === List(1))
    assert(List.reverse(Nil) === Nil)
  }

  test("List.append2") {
    assert(List.append2(list5)(list5) === List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
    assert(List.append2(list5)(list3) === List(1, 2, 3, 4, 5, 1, 2, 3))
    assert(List.append2(list2)(Nil) === List(1, 2))
    assert(List.append2(Nil)(list2) === List(1, 2))
  }

  test("List.flatten") {
    assert(List.concat(List(list1, list2, list3)) === List(1, 1, 2, 1, 2, 3))
    assert(List.concat(List(list5)) === list5)
    assert(List.concat(List(Nil)) === Nil)
  }

  test("List.add1") {
    assert(List.add1(list5) === List(2, 3, 4, 5, 6))
    assert(List.add1(Nil) === Nil)
  }

  test("List.doubleToString") {
    val list5 = List(1.0, 2.0, 3.0, 4.0, 5.0)
    assert(List.doubleToString(list5) === List("1.0", "2.0", "3.0", "4.0", "5.0"))
    assert(List.doubleToString(Nil) === Nil)
  }

  test("List.filter") {
    assert(List.filter(list5)(_ % 2 == 0) === List(2, 4))
    assert(List.filter(list5)(_ % 2 == 1) === List(1, 3, 5))
    assert(List.filter(Nil: List[Int])(_ % 2 == 1) === Nil)
  }

  test("List.flatMap") {
    assert(List.flatMap(list5)(i => List(i, i)) === List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
  }

  test("List.filter2") {
    assert(List.filter2(list5)(_ % 2 == 0) === List(2, 4))
    assert(List.filter2(list5)(_ % 2 == 1) === List(1, 3, 5))
    assert(List.filter2(Nil: List[Int])(_ % 2 == 1) === Nil)
  }

  test("List.addPairwise") {
    assert(List.addPairwise(list5, list5) === List(2, 4, 6, 8, 10))
    assert(List.addPairwise(list5, list3) === List(2, 4, 6))
    assert(List.addPairwise(list3, list5) === List(2, 4, 6))
    assert(List.addPairwise(Nil, list5) === Nil)
    assert(List.addPairwise(list5, Nil) === Nil)
  }

  test("List.hasSubsequence") {
    assert(List.hasSubsequence(list5, List(1, 2, 3)) === true)
    assert(List.hasSubsequence(list5, list5) === true)
    assert(List.hasSubsequence(list5, List(1)) === true)
    assert(List.hasSubsequence(list5, List(2, 3)) === true)
    assert(List.hasSubsequence(list5, List(2, 3, 4)) === true)
    assert(List.hasSubsequence(list5, List(1, 3)) === false)
    assert(List.hasSubsequence(list5, List(6)) === false)
    assert(List.hasSubsequence(list1, List(1)) === true)
    assert(List.hasSubsequence(list1, Nil) === true)
  }
}
