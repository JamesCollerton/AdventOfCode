import Utils._
import Direction._

import scala.collection.mutable.ArrayBuffer

object DayTwentyTwo {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyTwoTestInput.txt")
		solveOne(input)
	}

	def solveOne(grid: ArrayBuffer[Array[String]]): Unit = {
		// Find all of the coordinates where an infection is
		val infectedCoordinates = findInfectedCoordinates(grid)
		// Middle
		val startingCoordinates = (grid(0).length / 2, grid.length /2)
		// Start Moving
		solveOneStep(infectedCoordinates, startingCoordinates, Direction.North)
	}

	@annotation.tailrec
	def solveOneStep(infectedCoordinates: ArrayBuffer[(Int, Int)], currentCoordinates: (Int, Int), infectionCounter: Int, counter: Int, direction: Direction): Int = {
		return infectionCounter
		val (newCoordinates, newInfectionCounter, newDirection) = if(infectedCoordinates.contains(currentCoordinates)) {
			// Turn right
			val results = direction match {
				case North => 	((currentCoordinates._1 + 1, currentCoordinates._2), infectionCounter + 1, Direction.East)
				case East => 	((currentCoordinates._1, currentCoordinates._2 + 1), infectionCounter + 1, Direction.South)
				case South => 	((currentCoordinates._1 - 1, currentCoordinates._2), infectionCounter + 1, Direction.West)
				case West => 	((currentCoordinates._1, currentCoordinates._2 - 1), infectionCounter + 1, Direction.North)  
			}
			results 
		} else {
			(currentCoordinates, infectionCounter, direction)
		}
		solveOneStep(infectedCoordinates, newCoordinates, newInfectionCounter, counter - 1, newDirection)
	} 

	def findInfectedCoordinates(grid: ArrayBuffer[Array[String]]): ArrayBuffer[(Int, Int)] = {
		val infectedCoords = grid.zipWithIndex.flatMap{ case (row, i) => {
			row.zipWithIndex.flatMap{ case(field, j) => {
				if (field == "#")  Some( (j, i) )
				else None
			} } 
		} }
		infectedCoords
	}

}
