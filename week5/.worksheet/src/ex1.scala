object ex1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(56); 
  println("Welcome to the Scala worksheet");$skip(270); 

  def removeAt[T](xs: List[T], n: Int): List[T]=
  if ( n  < 0 || n >= xs.length ){ println(n + " is out of bound (0, " + (xs.length -1 ) +")" ); xs}
  else
    xs match {
       case List() => Nil
       case h :: t => if (n == 0) t else h :: removeAt(t, n -1)
    };System.out.println("""removeAt: [T](xs: List[T], n: Int)List[T]""");$skip(42); val res$0 = 
    
  removeAt(List('a','b','c','d'),0);System.out.println("""res0: List[Char] = """ + $show(res$0));$skip(36); val res$1 = 
  removeAt(List('a','b','c','d'),1);System.out.println("""res1: List[Char] = """ + $show(res$1));$skip(36); val res$2 = 
  removeAt(List('a','b','c','d'),3);System.out.println("""res2: List[Char] = """ + $show(res$2));$skip(36); val res$3 = 
  removeAt(List('a','b','c','d'),4);System.out.println("""res3: List[Char] = """ + $show(res$3));$skip(39); val res$4 = 
  removeAt(List('a','b','c','d'),-100);System.out.println("""res4: List[Char] = """ + $show(res$4));$skip(228); 
  
  def flatten(xs: List[Any]): List[Any] = {
    xs match {
      case List() => List()
      case h :: t => h match {
        case h: List[Any] => flatten(h) ++ flatten(t)
        case _ => h :: flatten(t)
      }
    }
  };System.out.println("""flatten: (xs: List[Any])List[Any]""");$skip(54); val res$5 = 

  flatten(List(List(1, 1), 2, List(3, List(5, 8))));System.out.println("""res5: List[Any] = """ + $show(res$5))}

}
