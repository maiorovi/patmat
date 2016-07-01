package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times count frequency of characters in text") {
    val result = times(convert("abcbadaddd"))

    assert(findACharAmt(result, 'a') == 3)
    assert(findACharAmt(result, 'b') == 2)
    assert(findACharAmt(result, 'c') == 1)
    assert(findACharAmt(result, 'd') == 4)
  }

  test("combine cut 2 elems, create fork with these elems and insert them in a list at appropriate position") {
    val list = List(
      Leaf('a', 1),
      Leaf('b', 2),
      Leaf('c', 2),
      Leaf('d', 5)
    )

    val tree = combine(list)

    assert( tree == tree)


  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("secret") {
    println(decode(frenchCode,secret).mkString(""))
  }

  test("encode with secret code") {
    println(encode(frenchCode)("abc".toList).toString())
  }

  test("decode with secret code") {
    println(decode(frenchCode, List(1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0)).toString())
  }


  private def convert(x:String):List[Char] = {

    var input = x.toCharArray
    def loop(i:Int, acc:List[Char]):List[Char] = if (i < input.length) loop(i+1, input(i)::acc) else acc

    loop(0, Nil)
  }

  private def findACharAmt(xs:List[(Char,Int)],ch:Char):Int = xs.find( x => x._1 == ch).map( z => z._2).get


}
