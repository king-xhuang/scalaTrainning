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
  
  test("combine of Nil or singleton") {
    val leaflist = List(Leaf('e', 1) )
    assert(combine(leaflist) === List( Leaf('e',1) ))
     assert(combine(Nil) === List( ))
  }
  
  test("until "){
     val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
     assert(until(singleton, combine)(leaflist).head === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t','x'), 7))
  }
  test ("convert t3 to codeTable "){
    new TestTrees{
      val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
      val t3 = until(singleton, combine)(leaflist).head
      println("code tree=" + t3) 
      
      val codetable = convert(t3)
      println("codeTable for t3=" + codetable)
      
    }
  }
  
   test ("encode test"){
    new TestTrees{
      println("encode test")
      val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
      val t3 = until(singleton, combine)(leaflist).head
      println("code tree=" + t3) 
      val chars= "text".toList
      val encoded = encode(t3)(chars)
      println("encode for '" + chars + "' =" + encoded )
      assert(encoded === List(0,1,0,0,1,0,1))
    }
  }
  
   test("createCodeTree 'aabbcccdddd' and encode/decode"){
     val chars = "aabbcccdddd";
     val tree = createCodeTree(chars.toList)
     println("aabbcccdddd" + " tree=" + tree)
     val codeTable = convert(tree)
     println("codeTable=" + codeTable)
     
     val text = "abc".toList
     val ed = encode(tree)(text)
     println(text + " encoded=" + ed)
     
     def testEncodDecode(text: List[Char]){
       assert(decode(tree, encode(tree)(text)) === text )
     }
     setPrint(true)
     val dc =  decode(tree, ed )
     println(text + " decoded=" + dc)
     
     assert(  dc === text )
     
     testEncodDecode("abd".toList)
     
     testEncodDecode("adb".toList)
     
     testEncodDecode("acdb".toList)
     
     testEncodDecode("accd".toList)
     
     testEncodDecode("bacd".toList)
     
     testEncodDecode("dbacd".toList)
     
     setPrint(false) 
     
   }
   
   test("createCodeTree 'ddddababcdcdc' and encode/decode"){
     val chars = "ddddababcdcdc";
     val tree = createCodeTree(chars.toList)
     println(chars + " tree=" + tree)
     val codeTable = convert(tree)
     println("codeTable=" + codeTable)
     
     val text = "abc".toList
     val ed = encode(tree)(text)
     println(text + " encoded=" + ed)
     
     def testEncodDecode(text: List[Char]){
       assert(decode(tree, encode(tree)(text)) === text )
     }
     setPrint(true)
     val dc =  decode(tree, ed )
     println(text + " decoded=" + dc)
     
     assert(  dc === text )
     
     testEncodDecode("abd".toList)
     
     testEncodDecode("adb".toList)
     
     testEncodDecode("acdb".toList)
     
     testEncodDecode("accd".toList)
     
     testEncodDecode("bacd".toList)
     
     testEncodDecode("dbacd".toList)
     
     setPrint(false) 
     
   }
  
  
   

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("decodedSecret"){
    new TestTrees{
      println("decodedSecret=" + decodedSecret)
    }
  }
}
