import Utils._
import Coordinates._

import scala.collection.mutable.ArrayBuffer

object DayNineteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayNineteenInput.txt")
		solveOne(input)
	}

	def solveOne(maze: ArrayBuffer[Array[String]]): Unit = {
		val start = findStart(maze)
		println("Start " + start)
		val characterList = solveOneStep(maze, new Coordinates(start, 0), new Coordinates(0, 1), new ArrayBuffer[String](), 0)
		println("Character list " + characterList.mkString(""))
	}

	def solveOneStep(maze: ArrayBuffer[Array[String]], position: Coordinates, direction: Coordinates, characterList: ArrayBuffer[String], counter: Int): ArrayBuffer[String] = {

		// If at edge then return
		if(	position.x + direction.x >= maze(0).length ||
			position.x + direction.x < 0 ||
			position.y + direction.y >= maze.length ||
			position.y + direction.y < 0 			) {
				return characterList		
		}

		// If on a blank space then return
		if( maze(position.y + direction.y)(position.x + direction.x) == "" ) return characterList

		// Go in current direction
		val newPosition = new Coordinates(position.x + direction.x, position.y + direction.y)

		// Mark old position
		maze(position.y)(position.x) = "0"

		// If find cross change direction
		val newDirection = if(maze(newPosition.y)(newPosition.x) == "+") {
			val newDirection = if(!Array("", "0").contains(maze(newPosition.y + 1)(newPosition.x))) { 
				// Down
				new Coordinates(0, 1)
			} else if(!Array("", "0").contains(maze(newPosition.y - 1)(newPosition.x))) { 
				// Up
				new Coordinates(0, -1)
			} else if(!Array("", "0").contains(maze(newPosition.y)(newPosition.x - 1))) {
				// Left
				new Coordinates(-1, 0)
			} else if(!Array("", "0").contains(maze(newPosition.y)(newPosition.x + 1))) { 
				// Right
				new Coordinates(1, 0)
			} else {
				println("Problem")
				direction
			}
			newDirection
		} else {
			direction
		}
		
		// If find letter
		if(!Array("-", "|", "+", "0").contains(maze(newPosition.y)(newPosition.x))){
			characterList += maze(newPosition.y)(newPosition.x)
		}

		//printMaze(maze, newPosition)		

		// Look for next direction
		solveOneStep(maze, newPosition, newDirection, characterList, counter + 1)
	}

	def printMaze(maze: ArrayBuffer[Array[String]], position: Coordinates): Unit = {
		for(i <- 0 to maze.length - 1) {
			val row = maze(i)
			for(j <- 0 to row.length - 1) {
				if(position.x == j && position.y == i) {
					print("*")
				} else {
					if(maze(i)(j) == "") print(".")
					print(maze(i)(j) + "")
				}
			}
			println()
		}
		println()
	}

	def findStart(maze: ArrayBuffer[Array[String]]): Int = {
		val firstIndexes = maze(0).zipWithIndex.filter(_._1 != "").map(_._2)
		firstIndexes(0)
	}

}
