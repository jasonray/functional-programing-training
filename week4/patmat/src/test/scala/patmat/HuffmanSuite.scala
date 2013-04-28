package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("append list examples") {
    new TestTrees {
      assert(List(1, 2) ++ List(3, 4) == List(1, 2, 3, 4))
      assert(List(1, 2) :+ 3 == List(1, 2, 3))
      assert(1 :: List(2, 3) == List(1, 2, 3))

      //replace 2 with 200
      val l = List(1, 2, 3)
      val newL = l updated (l indexOf 2, 200)
      assert(newL == List(1, 200, 3))

      //insert item into list
      val l2 = List(1, 3, 4)
      val index = 1;
      val newL2 = ((l2 take index) :+ 2) ++ (l2 drop index)
      assert(newL2 == List(1, 2, 3, 4))

    }
  }

  test("update freq of first element") {
    val initialCharCount = List[(Char, Int)](('a', 1), ('b', 2))
    val expectedResult = List[(Char, Int)](('a', 2), ('b', 2))
    val actualResult = updateFreq(initialCharCount, 'a')
    assert(actualResult == expectedResult, actualResult + " not equal to " + expectedResult)
  }
  test("update freq of last element") {
    val initialCharCount = List[(Char, Int)](('a', 1), ('b', 2))
    assert(updateFreq(initialCharCount, 'b') == List[(Char, Int)](('a', 1), ('b', 3)))
  }
  test("update freq of new element") {
    val initialCharCount = List[(Char, Int)](('a', 1), ('b', 2))
    val expectedResult = List[(Char, Int)](('a', 1), ('b', 2), ('c', 1))
    val actualResult = updateFreq(initialCharCount, 'c')
    assert(actualResult == expectedResult, actualResult + " not equal to " + expectedResult)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(t1.weight === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("times") {
    new TestTrees {
      assert(times(List('a', 'b', 'a')) == List(('a', 2), ('b', 1)))

      val result = times(List('d', 'a', 'b', 'c', 'd', 'd', 'c', 'a', 'c', 'd', 'd'));
      val expectedResult = List(('d', 5), ('a', 2), ('b', 1), ('c', 3));
      assert(result == expectedResult, result + " not equal to " + expectedResult)
    }
  }

  test("ordered list") {
    new TestTrees {
      val freqs = times(List('d', 'a', 'b', 'c', 'd', 'd', 'c', 'a', 'c', 'd', 'd'))
      val ordered = makeOrderedLeafList(freqs)

      assert(new Leaf('b', 1) === ordered(0));
      assert(new Leaf('a', 2) === ordered(1));
      assert(new Leaf('c', 3) === ordered(2));
      assert(new Leaf('d', 5) === ordered(3));

    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of empty list") {
    val emptyLeafList = List[Leaf]()
    assert(combine(emptyLeafList) === emptyLeafList)
  }
  test("combine of single item list") {
    val singleItemList = List[Leaf](Leaf('a', 1))
    assert(combine(singleItemList) === singleItemList)
  }
  test("combine of two item list") {
    val twoItemList = List[Leaf](Leaf('a', 1), Leaf('b', 1))
    assert(combine(twoItemList) === List(Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val phrase = "ab"
      assert(decode(t1, encode(t1)(phrase.toList)) === phrase.toList)
    }
  }

  test("decode and encode a text with repeating chars") {
    new TestTrees {
      val phrase = "aba"
      val encoded = encode(t1)(phrase.toList)
      println("encoded " + phrase + " to " + encoded);
      val decoded = decode(t1, encoded);
      println("decoded " + encoded + " to " + decoded + " (original=" + phrase.toList + ")");
      assert(decoded === phrase.toList)
    }
  }

  test("decode and encode a very short text should be identity, using quickEncode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("french decode") {
    new TestTrees {
      println("french decoded phrase: " + decodedSecret);
    }
  }

  test("add node to beginning of tree") {
    new TestTrees {
      val tree = List(Leaf('a', 2))
      val node = Leaf('b', 1)
      val expectedNewTree = List(Leaf('b', 1), Leaf('a', 2))
      val result = addNodeToTreeInOrder(node, tree);
      assert(result == expectedNewTree)
    }
  }

  test("add node to end of tree") {
    new TestTrees {
      val tree = List(Leaf('a', 1))
      val node = Leaf('b', 2)
      val expectedNewTree = List(Leaf('a', 1), Leaf('b', 2))
      val result = addNodeToTreeInOrder(node, tree);
      assert(result == expectedNewTree, result + " did not equal " + expectedNewTree)
    }
  }

  test("add node to middle of tree") {
    new TestTrees {
      val tree = List(Leaf('a', 1), Leaf('c', 3))
      val node = Leaf('b', 2)
      val expectedNewTree = List(Leaf('a', 1), Leaf('b', 2), Leaf('c', 3))
      val result = addNodeToTreeInOrder(node, tree);
      assert(result == expectedNewTree, result + " did not equal " + expectedNewTree)
    }
  }

  test("create code tree") {
    new TestTrees {
      val phrase = "mary had a little lamb".toList
      val codetree = createCodeTree(phrase)
      println("code tree: " + codetree);
    }
  }
}
