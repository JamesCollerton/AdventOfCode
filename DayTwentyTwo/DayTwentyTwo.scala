import Utils._
import Direction._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

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
		println("Number of infected " + solveOneStep(infectedCoordinates, startingCoordinates, 0, 1000000, Direction.North))
	}

	@annotation.tailrec
	def solveOneStep(infectedCoordinates: HashMap[(Int, Int), String], currentCoordinates: (Int, Int), infectionCounter: Int, counter: Int, direction: Direction.Value): Int = {
		if (counter == 0) return infectionCounter

		val (newCoordinates, newInfectionCounter, newDirection) = if(infectedCoordinates.contains(currentCoordinates) && infectedCoordinates(currentCoordinates) == "I") {

			// Square is infected 
			// 	- Turn right
			// 	- Flag it.

			// Flag
			infectedCoordinates(currentCoordinates) = "F" 
 
			// Turn right
			val results = direction match {
				case Direction.North => ((currentCoordinates._1 + 1, currentCoordinates._2), infectionCounter, Direction.East)
				case Direction.East => 	((currentCoordinates._1, currentCoordinates._2 + 1), infectionCounter, Direction.South)
				case Direction.South => ((currentCoordinates._1 - 1, currentCoordinates._2), infectionCounter, Direction.West)
				case Direction.West => 	((currentCoordinates._1, currentCoordinates._2 - 1), infectionCounter, Direction.North)  
			}
			results 
		} else if(infectedCoordinates.contains(currentCoordinates) && infectedCoordinates(currentCoordinates) == "F") {

			// Square is flagged
			// 	- Turn back
			// 	- Clean it
			
			// Clean (Remove from list)
			infectedCoordinates -= currentCoordinates

			// Turn Back
			val results = direction match {
				case Direction.North => ((currentCoordinates._1, currentCoordinates._2 + 1), infectionCounter, Direction.South)
				case Direction.East => 	((currentCoordinates._1 - 1, currentCoordinates._2), infectionCounter, Direction.West)
				case Direction.South => ((currentCoordinates._1, currentCoordinates._2 - 1), infectionCounter, Direction.North)
				case Direction.West => 	((currentCoordinates._1 + 1, currentCoordinates._2), infectionCounter, Direction.East)  
			}
			results 

		} else if(infectedCoordinates.contains(currentCoordinates) && infectedCoordinates(currentCoordinates) == "W") {

			// Square is weakened
			// 	- Go forward
			// 	- Infect it
			
			// Infect
			infectedCoordinates(currentCoordinates) = "I" 

			// Go forward
			val results = direction match {
				case Direction.North => ((currentCoordinates._1, currentCoordinates._2 - 1), infectionCounter + 1, Direction.North)
				case Direction.East => 	((currentCoordinates._1 + 1, currentCoordinates._2), infectionCounter + 1, Direction.East)
				case Direction.South => ((currentCoordinates._1, currentCoordinates._2 + 1), infectionCounter + 1, Direction.South)
				case Direction.West => 	((currentCoordinates._1 - 1, currentCoordinates._2), infectionCounter + 1, Direction.West)  
			}
			results 

		} else { 

			// Square is clean
			// 	- Turn left
			// 	- Weaken it
		
			// Weaken	
			infectedCoordinates(currentCoordinates) = "W" 

			// Turn left
			val results = direction match {
				case Direction.North => ((currentCoordinates._1 - 1, currentCoordinates._2), infectionCounter, Direction.West)
				case Direction.East => 	((currentCoordinates._1, currentCoordinates._2 - 1), infectionCounter, Direction.North)
				case Direction.South => ((currentCoordinates._1 + 1, currentCoordinates._2), infectionCounter, Direction.East)
				case Direction.West => 	((currentCoordinates._1, currentCoordinates._2 + 1), infectionCounter, Direction.South)  
			}
			results 
		}

		solveOneStep(infectedCoordinates, newCoordinates, newInfectionCounter, counter - 1, newDirection)
	} 

	def findInfectedCoordinates(grid: ArrayBuffer[Array[String]]): HashMap[(Int, Int), String] = {
		val infectedCoords = new HashMap[(Int, Int), String]
		grid.zipWithIndex.foreach{ case (row, i) => {
			row.zipWithIndex.foreach{ case(field, j) => {
				if (field == "#")  infectedCoords((j, i)) = "I"
			} } 
		} }
		infectedCoords
	}

}
