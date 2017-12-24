import Utils._
import Direction._

import scala.collection.mutable.ArrayBuffer

object DayTwentyTwo {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyTwoInput.txt")
		solveOne(input)
	}

	def solveOne(grid: ArrayBuffer[Array[String]]): Unit = {
		// Find all of the coordinates where an infection is
		val infectedCoordinates = findInfectedCoordinates(grid)
		// Middle
		val startingCoordinates = (grid(0).length / 2, grid.length /2)
		// Start Moving
		println("Number of infected " + solveOneStep(infectedCoordinates, startingCoordinates, 0, 10000, Direction.North))
	}

	@annotation.tailrec
	def solveOneStep(infectedCoordinates: ArrayBuffer[(Int, Int)], currentCoordinates: (Int, Int), infectionCounter: Int, counter: Int, direction: Direction.Value): Int = {
		if (counter == 0) return infectionCounter

//		println()
//		println("---------------------------------------------------------------")
//		println()
//
//		println("Infected Coordinates")
//		infectedCoordinates.foreach(coord => println(coord))
//		println("Current Coordinates " + currentCoordinates)
//		println("Infection counter " + infectionCounter)
//		println("Counter " + counter)
//		println("Direction " + direction)

		val (newCoordinates, newInfectionCounter, newDirection) = if(infectedCoordinates.contains(currentCoordinates)) {
			// Clean square
			infectedCoordinates -= currentCoordinates

//			println("Current Node is Infected")

			// Turn right
			val results = direction match {
				case Direction.North => ((currentCoordinates._1 + 1, currentCoordinates._2), infectionCounter, Direction.East)
				case Direction.East => 	((currentCoordinates._1, currentCoordinates._2 + 1), infectionCounter, Direction.South)
				case Direction.South => ((currentCoordinates._1 - 1, currentCoordinates._2), infectionCounter, Direction.West)
				case Direction.West => 	((currentCoordinates._1, currentCoordinates._2 - 1), infectionCounter, Direction.North)  
			}
			results 
		} else {
			// Infect square	
			infectedCoordinates += currentCoordinates

//			println("Current Node is Clean")

			// Turn left
			val results = direction match {
				case Direction.North => ((currentCoordinates._1 - 1, currentCoordinates._2), infectionCounter + 1, Direction.West)
				case Direction.East => 	((currentCoordinates._1, currentCoordinates._2 - 1), infectionCounter + 1, Direction.North)
				case Direction.South => ((currentCoordinates._1 + 1, currentCoordinates._2), infectionCounter + 1, Direction.East)
				case Direction.West => 	((currentCoordinates._1, currentCoordinates._2 + 1), infectionCounter + 1, Direction.South)  
			}
			results 
		}

//		println()
//		println("---------------------------------------------------------------")
//		println()

		solveOneStep(infectedCoordinates, newCoordinates, newInfectionCounter, counter - 1, newDirection)
	} 

	def findInfectedCoordinates(grid: ArrayBuffer[Array[String]]): ArrayBuffer[(Int, Int)] = {
		val infectedCoords = grid.zipWithIndex.flatMap{ case (row, i) => {
			row.zipWithIndex.flatMap{ case(field, j) => {
				if (field == "#")  Some( (j, i) )
				else None
			} } 
		} }
		infectedCoords.foreach(coord => println(coord))
		infectedCoords
	}

}
