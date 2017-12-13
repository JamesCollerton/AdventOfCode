import Utils._
import scala.collection.mutable.HashMap
                
object DayThirteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayThirteenInput.txt")
		solveOne(input)
	}

	def solveOne(fireWall: HashMap[Int, (Int, Int, Boolean)]): Unit = {
		val stopPosition = fireWall.map{ case(k, v) => k }.toArray.max
		println("Severity " + solveOneStep(0, fireWall, stopPosition))
	}

	def solveOneStep(currPos: Int, fireWall: HashMap[Int, (Int, Int, Boolean)], stopPosition: Int): Int = {
		if(currPos > stopPosition) return 0

		// Check if we're caught
		val caughtIncrement = if(fireWall.contains(currPos) && fireWall(currPos)._2 == 1) { 
			currPos * fireWall(currPos)._1
		} else {
			0
		}

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
			
		solveOneStep(currPos + 1, movedFireWall, stopPosition) + caughtIncrement
	}

}
