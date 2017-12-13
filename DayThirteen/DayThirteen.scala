import Utils._
import scala.collection.mutable.HashMap
                
object DayThirteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayThirteenInput.txt")
		solveTwo(input)
	}

	def solveTwo(fireWall: HashMap[Int, (Int, Int, Boolean)]): Unit = {
		val stopPosition = fireWall.map{ case(k, v) => k }.toArray.max
		println("Number of seconds delay  " + solveTwoStep(fireWall, stopPosition, 0))
	}

	@annotation.tailrec
	def solveTwoStep(fireWall: HashMap[Int, (Int, Int, Boolean)], stopPosition: Int, delay: Int): Int = {
		val fireWallClone = fireWall.clone
		val caught = solveOneStep(-delay, fireWall, stopPosition)  
		if(!caught) return delay;
		if(delay % 1000 == 0){
		println()
		println("CALLING NEXT: " + delay)
		}
		solveTwoStep(fireWallClone, stopPosition, delay + 1)
	}

	@annotation.tailrec
	def solveOneStep(currPos: Int, fireWall: HashMap[Int, (Int, Int, Boolean)], stopPosition: Int): Boolean = {

		// Reached end
		if(currPos > stopPosition) return false

		// Got caught
		if(fireWall.contains(currPos) && fireWall(currPos)._2 == 1) return true

		// Check if we're caught
		//val caughtIncrement = if(fireWall.contains(currPos) && fireWall(currPos)._2 == 1) { 
		//	println()
		//	println("Current Position " + currPos)
		//	true
		//} else {
		//	false
		//}

		val movedFireWall = fireWall
		fireWall.foreach{case(k, v)  => {
                        // Going forwards
                        if(v._3 == false) {
				// If we are at the end
				if(v._2 == v._1) {
					movedFireWall(k) = (v._1, v._2 - 1, true)
				} else {
					movedFireWall(k) = (v._1, v._2 + 1, v._3)
				}             
                        // Going backwards
                        } else {
				// If we are at the start
				if(v._2 == 1) {
					movedFireWall(k) = (v._1, v._2 + 1, false)
				} else {
					movedFireWall(k) = (v._1, v._2 - 1, v._3)
				}
                        }
		}}

		//println()
		//println("" + currPos)

		//println()
		//movedFireWall.foreach{ case(k, v) => println("Key " + k  + " Value " + v._1 + ", " + v._2 + ", " + v._3) }
			
		solveOneStep(currPos + 1, movedFireWall, stopPosition)
	}

}
