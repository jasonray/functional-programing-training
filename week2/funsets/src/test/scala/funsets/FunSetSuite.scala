package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val emptySet: Set = (x: Int) => false
    val infSet: Set = (x: Int) => true
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 0), "Singleton does not contain other element")
    }
  }
  test("singletonSet(1) does not contain 1") {
    new TestSets {
      assert(!contains(s1, 0), "Singleton does not contain other element")
    }
  }

  test("empty set def") {
    new TestSets {
      assert(!contains(emptySet, 0), "Empty set should not contain anything")
      assert(!contains(emptySet, 5), "Empty set should not contain anything")
      assert(!contains(emptySet, 10), "Empty set should not contain anything")
    }
  }
  test("inf set def") {
    new TestSets {
      assert(contains(infSet, 0), "Infinite set should contain everything")
      assert(contains(infSet, 5), "Infinite set should contain everything")
      assert(contains(infSet, 10), "Infinite set should contain everything")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("union with empty does not change") {
    new TestSets {
      val s = union(s1, emptySet)
      assert(contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  test("union with inf contains all") {
    new TestSets {
      val s = union(s1, infSet)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
    }
  }

  test("intersect") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)

      val s = intersect(s12, s23)

      assert(!contains(s, 1), "should not contain 1")
      assert(contains(s, 2), "should contain 2")
      assert(!contains(s, 3), "should not contain 3")
    }
  }

  test("intersect with empty is empty") {
    new TestSets {
      val s = intersect(s1, emptySet)
      assert(!contains(s, 1), "1")
      assert(!contains(s, 2), "2")
      assert(!contains(s, 3), "3")
    }
  }

  test("intersect with inf is same") {
    new TestSets {
      val s = intersect(s1, infSet)
      assert(contains(s, 1), "1")
      assert(!contains(s, 2), "2")
      assert(!contains(s, 3), "3")
    }
  }

  test("intersect with no overlap") {
    new TestSets {
      val s = intersect(s1, s2)

      assert(!contains(s, 1), "should not contain 1")
      assert(!contains(s, 2), "should not contain 2")
      assert(!contains(s, 3), "should not contain 3")
    }
  }

  test("diff") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s = diff(s12, s23)

      assert(contains(s, 1), "should contain 1")
      assert(!contains(s, 2), "should not contain 2")
      assert(!contains(s, 3), "should not contain 3")
    }
  }

  test("filter") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s1223 = union(s12, s23)

      assert(contains(s1223, 1), "should contain 1")
      assert(contains(s1223, 2), "should contain 2")
      assert(contains(s1223, 3), "should contain 3")

      val sf = filter(s1223, (x: Int) => x >= 2)
      assert(!contains(sf, 1), "should not contain 1")
      assert(contains(sf, 2), "should contain 2")
      assert(contains(sf, 3), "should contain 3")
    }
  }

  test("for each match") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)

      assert(forall(s123, (x: Int) => x > 0))
    }
  }

  test("for each no match") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)

      assert(!forall(s123, (x: Int) => x == 1))
    }
  }

  test("exists") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)

      assert(exists(s123, (x: Int) => x == 2))
    }
  }

  test("does not exist") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)

      assert(!exists(s123, (x: Int) => x == 4))
    }
  }

  test("map") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)

      val s = map(s123, (x: Int) => x * 2)
      assert(!contains(s, 1), "should not contain 1")
      assert(contains(s, 2), "should contain 2")
      assert(!contains(s, 3), "should not contain 3")
      assert(contains(s, 4), "should contain 4")
      assert(!contains(s, 5), "should not contain 5")
      assert(contains(s, 6), "should contain 6")
    }
  }

  test("div map") {
    new TestSets {
      val s24 = union(s2, s4)

      val s = map(s24, (x: Int) => x / 2)
      assert(contains(s, 1), "should contain 1")
      assert(contains(s, 2), "should contain 2")
      assert(!contains(s, 3), "should not contain 3")
      assert(!contains(s, 4), "should not contain 4")
      assert(!contains(s, 5), "should not contain 5")
      assert(!contains(s, 6), "should not contain 6")
    }
  }
}
