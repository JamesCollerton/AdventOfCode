import Utils._
import scala.collection.mutable.HashMap
                
object DayThirteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayThirteenInput.txt")
		solveTwo(input)
	}

	def solveTwo(fireWall: HashMap[Int, (Int, Int, Boolean)]): Unit = {
		val mathMap = fireWall.map({ case (k, v) => (k, v._1) }).toArray
		println("Math solve " + solveMath(mathMap, 0))
	}

	@annotation.tailrec	
	def solveMath(fireWall: Array[(Int, Int)], runningTotal: Int): Int = {
		val hitSum = fireWall.map(x => {
			val doubleResult = (x._1.toDouble - 2 * x._2.toDouble + 3) / (2 * (x._2.toDouble - 1))  
			if ( doubleResult == doubleResult.toInt.toDouble ) { 1 }
			else { 0 }
		}).sum
		if(hitSum == 0) return runningTotal + 1
		val incrFireWall = fireWall.map(x => (x._1 + 1, x._2))
		solveMath(incrFireWall, runningTotal + 1)
	}

}
