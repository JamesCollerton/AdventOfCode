import Utils._

import scala.collection.mutable.ArrayBuffer

object DayTwentyTwo {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyTwoTestInput.txt")
		solveOne(input)
	}

	def solveOne(grid: ArrayBuffer[Array[String]]): Unit = {
		// Find all of the coordinates where an infection is
		findInitialCoordinates(grid)
	}

	def findInitialCoordinates(grid: ArrayBuffer[Array[String]]): ArrayBuffer[(Int, Int)] = {
		val infectedCoords = grid.zipWithIndex.flatMap{ case (row, i) => {
			row.zipWithIndex.flatMap{ case(field, j) => {
				if (field == "#")  Some( (j, i) )
				else None
			} } 
		} }
		infectedCoords
	}

}
