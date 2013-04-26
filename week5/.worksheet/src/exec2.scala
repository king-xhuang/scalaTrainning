object exec2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(70); 
  println("Welcome to the Scala worksheet, pair tuple");$skip(26); val res$0 = 
  List(1, 2, 3) splitAt 1;System.out.println("""res0: (List[Int], List[Int]) = """ + $show(res$0));$skip(452); 

  def msort[T](xs: List[T])(f: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xt, y :: yt) => if (f(x, y)) x :: merge(xt, ys) else y :: merge(xs, yt)
        }
      val (fst, sst) = xs splitAt n
      merge(msort(fst)(f), msort(sst)(f))
    }
  };System.out.println("""msort: [T](xs: List[T])(f: (T, T) => Boolean)List[T]""");$skip(51); val res$1 = 

  msort(List(2, -3, 4, -10, 1))((x, y) => x < y);System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(48); val res$2 = 
  
  msort(List('a','c','b'))( (x,y) => x < y );System.out.println("""res2: List[Char] = """ + $show(res$2))}
}
