package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {

  import Main.countChange

  test("countChange: example given in instructions") {
    assert(countChange(4, List(1, 2)) === 3)
  }
  test("countChange: sorted CHF") {
    assert(countChange(300, List(5, 10, 20, 50, 100, 200, 500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301, List(5, 10, 20, 50, 100, 200, 500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300, List(500, 5, 50, 100, 20, 200, 10)) === 1022)
  }

  test("countChange: for 2 [1,2]") {
    assert(countChange(2, List(1, 2)) === 2)
  }
  test("countChange: for 2 [2,1]") {
    assert(countChange(2, List(2, 1)) === 2)
  }
  test("countChange: no coins") {
    assert(countChange(2, List()) === 0)
  }
  test("countChange: no change needed, not sure of correct behavior for this") {
    assert(countChange(0, List(2, 1)) === 1)
  }

  test("countChange: for only 1 type of coin") {
    assert(countChange(100, List(1)) === 1)
  }
  test("countChange: for only 1 type of coin unable to make change") {
    assert(countChange(5, List(2)) === 0)
  }
  test("countChange: unable to make change") {
    assert(countChange(1, List(2, 3, 4)) === 0)
  }

}
