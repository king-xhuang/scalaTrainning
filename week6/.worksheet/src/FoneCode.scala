 
import scala.io.Source

object FoneCode {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(87); 
  println("Welcome to the Scala worksheet");$skip(182); 
//  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
 val in = Source.fromFile("c:/scalaTrainning/week6/linuxwords.txt");System.out.println("""in  : scala.io.BufferedSource = """ + $show(in ));$skip(33); 
  val words = in.getLines.toList
  
  type Word = String;System.out.println("""words  : List[String] = """ + $show(words ));$skip(113); 

  val dictionary: List[Word] = words filter (word => word forall (chr => chr.isLetter));System.out.println("""dictionary  : List[FoneCode.Word] = """ + $show(dictionary ));$skip(165); 

  val mnem = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ");System.out.println("""mnem  : scala.collection.immutable.Map[Char,String] = """ + $show(mnem ));$skip(96); 
  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit;System.out.println("""charCode  : Map[Char,Char] = """ + $show(charCode ));$skip(72); 

  def wordCode(word: Word): String =
    word.toUpperCase map charCode;System.out.println("""wordCode: (word: FoneCode.Word)String""");$skip(102); 

  val wordsForNum: Map[String, Seq[String]] =
    dictionary groupBy wordCode withDefaultValue Seq();System.out.println("""wordsForNum  : Map[String,Seq[String]] = """ + $show(wordsForNum ));$skip(272); 

  def encode(number: String): Set[List[Word]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet;System.out.println("""encode: (number: String)Set[List[FoneCode.Word]]""");$skip(86); 

  def translate(number: String): Set[Word] =
    encode(number) map (_ mkString " ");System.out.println("""translate: (number: String)Set[FoneCode.Word]""");$skip(23); val res$0 = 
  encode("6787486089");System.out.println("""res0: Set[List[FoneCode.Word]] = """ + $show(res$0));$skip(19); val res$1 = 
  translate("678");System.out.println("""res1: Set[FoneCode.Word] = """ + $show(res$1))}
}
