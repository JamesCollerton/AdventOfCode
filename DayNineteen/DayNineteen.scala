import Utils._
import Coordinates._

import scala.collection.mutable.ArrayBuffer

object DayNineteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayNineteenTestInput.txt")
		solveOne(input)
	}

	def solveOne(maze: ArrayBuffer[Array[String]]): Unit = {
		val start = findStart(maze)
		println("Start " + start)
	}

	def solveOneStep(maze: ArrayBuffer[Array[String]], position: Coordinates, direction: Coordinates, characterList: Array[String]): Array[String] = {
		// If at edge then return
		if(	position.x + direction.x >= maze(0).length ||
			position.x + direction.x < 0 ||
			position.y + direction.y >= maze.length ||
			position.y + direction.y < 0 			) {
				return characterList		
		}

		// Go in current direction
		val newPosition = Coordinates(position.x + direction.x, position.y + direction.y)

		// If find cross
		
		// If find letter

		// Look for next direction
		solveOneStep(maze, newPosition, newDirection, characterList)
	}

	def findStart(maze: ArrayBuffer[Array[String]]): Int = {
		val firstIndexes = maze(0).zipWithIndex.filter(_._1 != "").map(_._2)
		firstIndexes(0)
	}

}
