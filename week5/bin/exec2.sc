object exec2 {
  println("Welcome to the Scala worksheet, pair tuple")
                                                  //> Welcome to the Scala worksheet, pair tuple
  List(1, 2, 3) splitAt 1                         //> res0: (List[Int], List[Int]) = (List(1),List(2, 3))

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
  }                                               //> msort: [T](xs: List[T])(f: (T, T) => Boolean)List[T]

  msort(List(2, -3, 4, -10, 1))((x, y) => x < y)  //> res1: List[Int] = List(-10, -3, 1, 2, 4)
  
  msort(List('a','c','b'))( (x,y) => x < y )      //> res2: List[Char] = List(a, b, c)
}