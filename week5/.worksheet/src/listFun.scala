object listFun {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(60); 
  println("Welcome to the Scala worksheet");$skip(103); 
  
  def mapFun[T,U] (xs: List[T], f: T=>U): List[U] =
   (xs foldRight List[U]())((t,u) => f(t) :: u);System.out.println("""mapFun: [T, U](xs: List[T], f: T => U)List[U]""");$skip(81); 
  
  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ( (x,y) => y + 1);System.out.println("""lengthFun: [T](xs: List[T])Int""");$skip(31); 
    
  val l1 = List(1,2,3,4);System.out.println("""l1  : List[Int] = """ + $show(l1 ));$skip(43); 
  val l2 = List("apple", "orange", "pear");System.out.println("""l2  : List[String] = """ + $show(l2 ));$skip(33); val res$0 = 
  mapFun[Int, Int] (l1, x=> x+x);System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(37); val res$1 = 
  mapFun[String, Int] (l2, _.length);System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(59); val res$2 = 
  mapFun[String, (String,Int)] (l2, (x=> ( x, x.length )));System.out.println("""res2: List[(String, Int)] = """ + $show(res$2));$skip(24); val res$3 = 
  lengthFun[String](l2);System.out.println("""res3: Int = """ + $show(res$3))}
  
}
