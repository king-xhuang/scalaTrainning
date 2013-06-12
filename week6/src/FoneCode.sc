 
import scala.io.Source

object FoneCode {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
//  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
 val in = Source.fromFile("c:/scalaTrainning/week6/linuxwords.txt")
                                                  //> in  : scala.io.BufferedSource = non-empty iterator
  val words = in.getLines.toList                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, ab
                                                  //| andoned, abandoning, abandonment, abandons, abase, abased, abasement, abasem
                                                  //| ents, abases, abash, abashed, abashes, abashing, abasing, abate, abated, aba
                                                  //| tement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbo
                                                  //| t, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbre
                                                  //| viation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, abducted
                                                  //| , abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abel
                                                  //| ian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, 
                                                  //| abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abh
                                                  //| orrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, 
                                                  //| Abilene, abilities, ability, abject, abjection, abjections, abjectly, abject
                                                  //| ness, abjure, abjured, abjures, abjuring, ablate, ablated, ablates, ablating
                                                  //| , ablation, ablative, ab
                                                  //| Output exceeds cutoff limit.
  
  type Word = String

  val dictionary: List[Word] = words filter (word => word forall (chr => chr.isLetter))
                                                  //> dictionary  : List[FoneCode.Word] = List(Aarhus, Aaron, Ababa, aback, abaft,
                                                  //|  abandon, abandoned, abandoning, abandonment, abandons, abase, abased, abase
                                                  //| ment, abasements, abases, abash, abashed, abashes, abashing, abasing, abate,
                                                  //|  abated, abatement, abatements, abater, abates, abating, Abba, abbe, abbey, 
                                                  //| abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbrevi
                                                  //| ating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdominal, abdu
                                                  //| ct, abducted, abduction, abductions, abductor, abductors, abducts, Abe, abed
                                                  //| , Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrat
                                                  //| ions, abet, abets, abetted, abetter, abetting, abeyance, abhor, abhorred, ab
                                                  //| horrent, abhorrer, abhorring, abhors, abide, abided, abides, abiding, Abidja
                                                  //| n, Abigail, Abilene, abilities, ability, abject, abjection, abjections, abje
                                                  //| ctly, abjectness, abjure, abjured, abjures, abjuring, ablate, ablated, ablat
                                                  //| es, ablating, ablation, 
                                                  //| Output exceeds cutoff limit.

  val mnem = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ")                                //> mnem  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GHI
                                                  //| , 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit
                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -
                                                  //| > 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5,
                                                  //|  B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -
                                                  //| > 9, S -> 7)

  def wordCode(word: Word): String =
    word.toUpperCase map charCode                 //> wordCode: (word: FoneCode.Word)String

  val wordsForNum: Map[String, Seq[String]] =
    dictionary groupBy wordCode withDefaultValue Seq()
                                                  //> wordsForNum  : Map[String,Seq[String]] = Map(63972278 -> List(newscast), 292
                                                  //| 37638427 -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> List
                                                  //| (allying), 862532733 -> List(uncleared), 365692259 -> List(enjoyably), 86843
                                                  //| 7 -> List(unties), 33767833 -> List(deportee), 742533 -> List(picked), 33646
                                                  //| 46489 -> List(femininity), 3987267346279 -> List(extraordinary), 7855397 -> 
                                                  //| List(pulleys), 67846493 -> List(optimize), 4723837 -> List(grafter), 386583 
                                                  //| -> List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 847827 
                                                  //| -> List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curlicue), 8
                                                  //| 4863372658 -> List(thunderbolt), 46767833 -> List(imported), 26437464 -> Lis
                                                  //| t(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(spoolers),
                                                  //|  46636233 -> List(homemade), 7446768759 -> List(rigorously), 74644647 -> Lis
                                                  //| t(ringings), 633738 -> List(offset), 847825 -> List(visual), 772832 -> List(
                                                  //| Pravda), 4729378 -> List
                                                  //| Output exceeds cutoff limit.

  def encode(number: String): Set[List[Word]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet                                       //> encode: (number: String)Set[List[FoneCode.Word]]

  def translate(number: String): Set[Word] =
    encode(number) map (_ mkString " ")           //> translate: (number: String)Set[FoneCode.Word]
  encode("6787486089")                            //> res0: Set[List[FoneCode.Word]] = Set()
  translate("678")                                //> res1: Set[FoneCode.Word] = Set(opt)
}