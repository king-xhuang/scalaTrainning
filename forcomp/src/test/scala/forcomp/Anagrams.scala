package forcomp

import common._
import scala.collection.immutable.Map

object Anagrams {
  val debug = false
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /**
   * Converts the word into its character occurence list. Implemented with span
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrencesV1(w: Word): Occurrences = {
    def runLen[Char](w1: List[Char]): List[(Char, Int)] =
      w1 match {
        case Nil => Nil
        case c :: t => val (t1, t2) = w1.span((c1) => c1 == c); (c, t1.length) :: runLen(t2)

      }
    val lowcase = for (c <- w) yield (if (c.isUpper) c.toLower else c)
    val sortedList: List[Char] = lowcase.toList.sorted[Char]
    runLen(sortedList)
  }
  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val lowcase = for (c <- w) yield (if (c.isUpper) c.toLower else c)
    val grouped = lowcase.groupBy(c => c).toList
    val occ = for ((k, s) <- grouped) yield (k, s.length)
    occ.sortBy[Char](kv => kv._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    val letters = for (w <- s.toList; c <- w.toList if c.isLetter) yield c
    //println(letters.toString)
    wordOccurrences(letters.mkString)
  }
  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {

    dictionary.groupBy(w => wordOccurrences(w))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences.get(wordOccurrences(word)).getOrElse(List())

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def subCombinations(acc: List[Occurrences], charOcc: (Char, Int)): List[Occurrences] = {
      val index = occurrences.indexOf(charOcc) + 1
      //println(index)
      val sub = occurrences.drop(index)
      // println(sub)
      if (sub.isEmpty) acc
      else {
        val next = sub.head
        val comb = for (ao <- acc; f <- 0 to next._2) yield (next._1, f) :: ao
        if (index == occurrences.length - 1) comb
        else
          subCombinations(comb, next)
      }
    }
    if (occurrences == Nil) List(Nil)
    else {
      val ll = for (oc <- occurrences; f <- 0 to oc._2) yield {
        val o = List((oc._1, f));
        //println((oc._1, f))
        subCombinations(List(o), oc)
      }
      for (l <- ll; o <- l) yield o.reverse.filter(x => x._2 != 0)
    }

  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val i = for (x1 <- x) yield {
      val cm = y.filter(y1 => y1._1 == x1._1)
      if (cm.isEmpty) x1
      else (x1._1, x1._2 - cm.head._2)
    }
    i.filter(x => x._2 > 0)
  }

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def findNext(i: Int, subOcc: Occurrences, wlAcc: List[Sentence]): List[Sentence] = {
      printInd(i, "wlAcc=" + wlAcc + ", occ=" + subOcc + ", total occ#=" + totalOcc(subOcc))
      if (subOcc.isEmpty) wlAcc
      else {
        val lo = combinations(subOcc)
        val lls = for (o <- lo) yield {
          val lw: List[Word] = dictionaryByOccurrences.getOrElse(o, Nil)
          printInd(i, "#  find for " + o)
          if (lw != Nil) {
            printInd(i, "  lw=" + lw)
            val ls = for (w <- lw; ps <- wlAcc) yield w :: ps
            printInd(i, "  ls=" + ls)
            findNext(i + 1, subtract(subOcc, o), ls)
            //List()
          } else {
            //printInd(i, "  lw=" + lw)            
            List()
          }
            
        }
        for (ls <- lls; s <- ls) yield s

      }
    }
    if (sentence.isEmpty) List(Nil)
    else {
      val so = sentenceOccurrences(sentence)
      println(0, "so=" + so)       
      findNext(0, so, List(Nil))   
          
    }

  }
  def sentenceAnagramsV1(sentence: Sentence): List[Sentence] = {
    def findNext(i: Int, subOcc: Occurrences, wlAcc: List[Sentence]): List[Sentence] = {
      printInd(i, "wlAcc=" + wlAcc + ", occ=" + subOcc + ", total occ#=" + totalOcc(subOcc))
      if (subOcc.isEmpty) wlAcc
      else {
        val lo = combinations(subOcc)
        val lls = for (o <- lo) yield {
          val lw: List[Word] = dictionaryByOccurrences.getOrElse(o, Nil)
          printInd(i, "#  find for " + o)
          if (lw != Nil) {
            printInd(i, "  lw=" + lw)
            val ls = for (w <- lw; ps <- wlAcc) yield w :: ps
            printInd(i, "  ls=" + ls)
            findNext(i + 1, subtract(subOcc, o), ls)
            //List()
          } else {
            //printInd(i, "  lw=" + lw)            
            List()
          }
            
        }
        for (ls <- lls; s <- ls) yield s

      }
    }
    if (sentence.isEmpty) List(Nil)
    else {
      val so = sentenceOccurrences(sentence)
      println(0, "so=" + so)
      val lo = combinations(so)
      val llw = for (o <- lo) yield {
        val lw: List[Word] = dictionaryByOccurrences.getOrElse(o, Nil)

        if (lw != Nil) {
          val ls = for (w <- lw) yield List(w);
          //println(o + "," + ls) ; 
          findNext(0, subtract(so, o), ls)
        } else List()
      }
      // println("llw=" + llw)
      for (ls <- llw; s <- ls) yield s.reverse
      
    }

  }
  def totalOcc1(o: Occurrences): Int ={
    val ia = for (co <- o) yield co._2
    0::ia reduceLeft (_ + _)
  }
  def totalOcc(o: Occurrences): Int ={     
    ( o foldLeft 0 ) ((x,y) => x + y._2)
  }
  def printInd(i: Int, a: Any) {
    if (debug) {
      for (s <- 1 to i*2) print(i)
      println(a)
    }
  }

}
