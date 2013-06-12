import scala.math.Ordering

object exec4 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

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
  }                                               //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]
  msort(List(2, -3, 4, -10, 1))                   //> res0: List[Int] = List(-10, -3, 1, 2, 4)

  msort(List('a', 'c', 'b'))                      //> res1: List[Char] = List(a, b, c)

  def pack[T](xs: List[T]): List[List[T]] =
    xs match {
      case Nil => Nil
      case x :: xt => val (y1, y2) = xs span (y => y == x); y1 :: pack(y2)
    }                                             //> pack: [T](xs: List[T])List[List[T]]

  pack(List('a', 'a', 'b', 'b', 'b', 't', 't'))   //> res2: List[List[Char]] = List(List(a, a), List(b, b, b), List(t, t))

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (t => (t.head, t.length))        //> encode: [T](xs: List[T])List[(T, Int)]

  encode(List('a', 'a', 'b', 'b', 'b', 't', 't')) //> res3: List[(Char, Int)] = List((a,2), (b,3), (t,2))

  // more efficient version of encode
  def encode2[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xt => val (y1, y2) = xs span (e => e == x); (x, y1.length) :: encode(y2)
  }                                               //> encode2: [T](xs: List[T])List[(T, Int)]

  encode2(List('a', 'a', 'b', 'b', 'b', 't', 't'))//> res4: List[(Char, Int)] = List((a,2), (b,3), (t,2))
}