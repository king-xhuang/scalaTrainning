object listFun {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def mapFun[T,U] (xs: List[T], f: T=>U): List[U] =
   (xs foldRight List[U]())((t,u) => f(t) :: u)   //> mapFun: [T, U](xs: List[T], f: T => U)List[U]
  
  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ( (x,y) => y + 1)            //> lengthFun: [T](xs: List[T])Int
    
  val l1 = List(1,2,3,4)                          //> l1  : List[Int] = List(1, 2, 3, 4)
  val l2 = List("apple", "orange", "pear")        //> l2  : List[String] = List(apple, orange, pear)
  mapFun[Int, Int] (l1, x=> x+x)                  //> res0: List[Int] = List(2, 4, 6, 8)
  mapFun[String, Int] (l2, _.length)              //> res1: List[Int] = List(5, 6, 4)
  mapFun[String, (String,Int)] (l2, (x=> ( x, x.length )))
                                                  //> res2: List[(String, Int)] = List((apple,5), (orange,6), (pear,4))
  lengthFun[String](l2)                           //> res3: Int = 3
  
}