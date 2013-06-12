import scala.math.Ordering

object exec4 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(87); 
  println("Welcome to the Scala worksheet");$skip(466); 

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xt, y :: yt) => if (ord.lt(x, y)) x :: merge(xt, ys) else y :: merge(xs, yt)
        }
      val (fst, sst) = xs splitAt n
      merge(msort(fst)(ord), msort(sst)(ord))
    }
  };System.out.println("""msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]""");$skip(32); val res$0 = 
  msort(List(2, -3, 4, -10, 1));System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(31); val res$1 = 

  msort(List('a', 'c', 'b'));System.out.println("""res1: List[Char] = """ + $show(res$1));$skip(164); 

  def pack[T](xs: List[T]): List[List[T]] =
    xs match {
      case Nil => Nil
      case x :: xt => val (y1, y2) = xs span (y => y == x); y1 :: pack(y2)
    };System.out.println("""pack: [T](xs: List[T])List[List[T]]""");$skip(50); val res$2 = 

  pack(List('a', 'a', 'b', 'b', 'b', 't', 't'));System.out.println("""res2: List[List[Char]] = """ + $show(res$2));$skip(92); 

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (t => (t.head, t.length));System.out.println("""encode: [T](xs: List[T])List[(T, Int)]""");$skip(52); val res$3 = 

  encode(List('a', 'a', 'b', 'b', 'b', 't', 't'));System.out.println("""res3: List[(Char, Int)] = """ + $show(res$3));$skip(209); 

  // more efficient version of encode
  def encode2[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xt => val (y1, y2) = xs span (e => e == x); (x, y1.length) :: encode(y2)
  };System.out.println("""encode2: [T](xs: List[T])List[(T, Int)]""");$skip(53); val res$4 = 

  encode2(List('a', 'a', 'b', 'b', 'b', 't', 't'));System.out.println("""res4: List[(Char, Int)] = """ + $show(res$4))}
}
