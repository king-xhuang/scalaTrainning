package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
      def p(col: Int, row: Int): Int = {
       //println("enter " + col + ", " + row)
       if (col == 0) 1
       else if (col < 0 ) 0
       else	if (row == 0 ) 0          
       else   p(col -1, row -1) + p(col, row -1) 
       
     }
     p(c, r)
  }
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var open = 0
     var close = 0
     var unmatch = 0
     def exam(head: Char, tail: List[Char]) {
       if (head.equals('('))          
          {     
          	open = open + 1  	
            unmatch = unmatch + 1
            //println(head + " open=" + open + " close=" + close + " unmatch=" + unmatch)
           }
    	   else if  (head.equals(')')){    	     
    	     close = close + 1
    	     unmatch =   if (open >= close) unmatch -1 else unmatch
    	     //println(head + " open=" + open + " close=" + close + " unmatch=" + unmatch)
    	   }
    	   else  Nil
    	   // print( " " )
       if (!tail.isEmpty) exam(tail.head, tail.tail)
         
     }
     if (chars.isEmpty) true
     else
       exam(chars.head, chars.tail)
       ( open == close ) && (unmatch == 0)
       
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
       var count = 0      
       var level = 0
       def calNextList(coins: List[Int]){
         pLevel("-- " + coins)
         if (!coins.isEmpty){   
           if (coins.head <= money){
        	   calCount(money, coins.head, coins.tail)
        	   pLevel("-- count=" + count)
           }
           calNextList(coins.tail)
         }
         else Nil    
       }
       def pLevel(msg: String){
         for (i <- 0 to level) print("  "); println(msg)
       }
       def examChange(coinsList: List[Int], coinCountList: List[Int], changeCount: Int): Int = {
         val cb = List.fill(coinCountList.size)(0)
         if ( (calTotal(cb, coinsList, 0)) == money ) 1
         else 0
        
       }
       def getNextCB(coinCountList: List[Int], oneCB: List[Int]): List[Int] = {
         if (coinCountList == oneCB) Nil
         else{
           Nil
         }
         
       }
       def allCB(coinCountList: List[Int]) {
         def oneCB(oneCBList: List[Int], restcoinCountList: List[Int]){
           if (restcoinCountList.isEmpty)  {
             val oc= oneCBList.reverse
             println(oc)
             if (calTotal(oc, coins, 0) == money) count = count + 1
             else count
           }
           else {
              var headVal = restcoinCountList.head
              
              for ( i <- 0 to headVal ){
                val ss = i :: oneCBList
                oneCB( ss, restcoinCountList.tail)
              }
                
           } 
         } 	  
         oneCB(Nil, coinCountList)
      
        
        
       }
       def calCoinCountList(coinsList: List[Int], coinCountList: List[Int]): List[Int] = {
          if (coinsList.isEmpty) coinCountList
          else calCoinCountList(coinsList.tail, (money/coinsList.head) :: coinCountList)
       }
       def calTotal(coinCountList: List[Int], coinsList: List[Int], acc: Int): Int = {
         if (acc > money) acc
         else
         if (coinCountList.isEmpty) {
            println(" total = " + acc)
            acc
          }
          else { 
            val cc = coinCountList.head
            val cv = coinsList.head
            print(cc + " x " + cv + ", " ) 
            calTotal(coinCountList.tail, coinsList.tail, acc + ( cc *  cv ))
          }
       }
       
       
       def calCount(amount: Int, oneCoin: Int, otheCoins: List[Int]) {
         level =level + 1
         pLevel("amount=" + amount + " coin:" + oneCoin + " otheCoins:" + otheCoins)
         var hasOne = false
         if (amount%oneCoin == 0) {     
           hasOne = true
           count = count + 1 
           pLevel("add one for " +  amount + ", " + oneCoin + " count=" + count)
         }
         else Nil
         pLevel("count=" + count)
         if (!otheCoins.isEmpty){
           pLevel("continue on " + otheCoins )
 //           calForRemain(amount,  oneCoin, otheCoins.head, otheCoins.tail)
               
              var remain = amount - oneCoin //if ( hasOne ) amount  else amount - oneCoin
	          while ( remain  > 0 ){
		          pLevel("remain=" + remain ) 
		          calCount(remain, otheCoins.head, otheCoins.tail)
		          var rest = otheCoins.tail
		          while(!rest.isEmpty){
		              calCount(remain, rest.head,  rest.tail )   
		              rest = rest.tail
	              }
		          remain = remain - oneCoin
		          pLevel("count=" + count)
	          }         
         
         }else {
           level = level - 1
           Nil
         }
       }
       
       def calForRemain(remain: Int, oneCoin: Int, head: Int, otheCoins: List[Int]) {
           val rm = remain - oneCoin
           if (rm > 0){
             
		            calCount(rm, head, otheCoins)
		            calForRemain(rm, oneCoin, head, otheCoins)
             
           }
       }
       
        println(coins)
        calNextList(coins.sorted.reverse) 
 
       
       
//        val coinChangeList = calCoinCountList(coins, List[Int]()).reverse
//        println("change list " + coinChangeList)
// 
//        allCB(coinChangeList)
        count
    
  }
}
