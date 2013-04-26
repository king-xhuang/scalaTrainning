object ex1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def removeAt[T](xs: List[T], n: Int): List[T]=
  if ( n  < 0 || n >= xs.length ){ println(n + " is out of bound (0, " + (xs.length -1 ) +")" ); xs}
  else
    xs match {
       case List() => Nil
       case h :: t => if (n == 0) t else h :: removeAt(t, n -1)
    }                                             //> removeAt: [T](xs: List[T], n: Int)List[T]
    
  removeAt(List('a','b','c','d'),0)               //> res0: List[Char] = List(b, c, d)
  removeAt(List('a','b','c','d'),1)               //> res1: List[Char] = List(a, c, d)
  removeAt(List('a','b','c','d'),3)               //> res2: List[Char] = List(a, b, c)
  removeAt(List('a','b','c','d'),4)               //> 4 is out of bound (0, 3)
                                                  //| res3: List[Char] = List(a, b, c, d)
  removeAt(List('a','b','c','d'),-100)            //> -100 is out of bound (0, 3)
                                                  //| res4: List[Char] = List(a, b, c, d)
  
  def flatten(xs: List[Any]): List[Any] = {
    xs match {
      case List() => List()
      case h :: t => h match {
        case h: List[Any] => flatten(h) ++ flatten(t)
        case _ => h :: flatten(t)
      }
    }
  }                                               //> flatten: (xs: List[Any])List[Any]

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res5: List[Any] = List(1, 1, 2, 3, 5, 8)

}