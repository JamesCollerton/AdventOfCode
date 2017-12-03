import math._

object SolveGrid {

	def calculateDistance(start: (Int, Int), end: (Int, Int)): Int = {
		Math.abs(start._1 - end._1) + Math.abs(start._2 - end._2)
	}

}
