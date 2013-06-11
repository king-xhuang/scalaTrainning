package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }
  trait Level6 extends SolutionChecker {
    val level =
      """-----oooooo
      |-----o--ooo
      |-----o--ooooo
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo
      |------ooooo
      |------ooooo
      |-------ooo""".stripMargin

    val optsolution = List() //TODO
  }
  trait Level33a extends   SolutionChecker {
    val level =
      """-----oo-ooo----        
  |-----oooooo----        
  |ooo---oo-ooooo-        
  |oSooooooo--oo--        
  |-----oo-oo-ooo-        
  |-----oooooo-oo-        
  |ooo--oooooo-ooo        
  |o-oooo-o--ooo-T        
  |ooo--ooo---oooo        
  |ooo---------ooo""".stripMargin
   val optsolution =List() //TODO
  }

  trait Level33b extends SolutionChecker {
    val level =
      """-----oo-ooo----        
  |-----ooooooo---        
  |ooo---oo-ooooo-        
  |ooooooooo--oo--        
  |-----oo-oo-ooo-        
  |-----oooooo-oo-        
  |ooo--oooooo-ooo        
  |oToooo-o--ooo-S        
  |ooo--ooo---oooo        
  |ooo---------ooo""".stripMargin
  
  val optsolution =List() //TODO
  }
  
  test("isLegal level 1") {
    new Level1 {
      val b1 = Block(Pos(1, 1), Pos(1, 2))
      assert(b1.isLegal == true)
      val b2 = Block(Pos(-1, 1), Pos(1, 2))
      assert(b2.isLegal == false)

      assert(Block(Pos(1, -1), Pos(1, 2)).isLegal == false)
      assert(Block(Pos(0, 2), Pos(0, 3)).isLegal == false)
      assert(Block(Pos(2, 0), Pos(2, 1)).isLegal == true)
      assert(Block(Pos(2, 8), Pos(2, 9)).isLegal == false)
      assert(Block(Pos(2, 9), Pos(3, 9)).isLegal == false)
      assert(Block(Pos(2, 8), Pos(3, 8)).isLegal == true)
      assert(Block(Pos(4, 8), Pos(4, 9)).isLegal == true)
      assert(Block(Pos(4, 8), Pos(5, 8)).isLegal == true)
      assert(Block(Pos(2, 1), Pos(3, 1)).isLegal == true)
      assert(Block(Pos(3, 0), Pos(3, 1)).isLegal == false)
      assert(Block(Pos(4, 5), Pos(5, 5)).isLegal == false)
      assert(Block(Pos(4, 6), Pos(5, 6)).isLegal == true)
      assert(Block(Pos(4, 9), Pos(5, 9)).isLegal == false)
      assert(Block(Pos(4, 10), Pos(5, 10)).isLegal == false)
    }
  }
  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }
  test("findChar -T level 1") {
    new Level1 {
      assert(goal == Pos(4, 7))
    }
  }
  test("findChar -S level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("optimal solution for level 1") {

    new Level1 {
      println("optimal solution for level 1 " + solution)
      assert(solve(solution) == Block(goal, goal))
      assert(solution.length == optsolution.length)
    }
  }

  test("optimal solution length for level 6") {
    new Level6 {
      println("optimal solution move for level 6 " + solution)
      println("optimal solution length for level 6 " + solution.size)
      assert(solve(solution) == Block(goal, goal))
    }
  }
  
  
  test("optimal solution length for level 33a") {
    new Level33a {
      println("optimal solution move for level 33a " + solution)
      println("optimal solution length for level 33a " + solution.size)
      assert(solve(solution) == Block(goal, goal))
    }
  }
  
  
   test("optimal solution length for level 33b") {
    new Level33b {
      println("optimal solution move for level 33b " + solution)
      println("optimal solution length for level 33b " + solution.size)
      assert(solve(solution) == Block(goal, goal))
    }
  }
}
