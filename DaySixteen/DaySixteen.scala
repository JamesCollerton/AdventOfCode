import Utils._

import scala.collection.mutable.HashMap

object DaySixteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DaySixteenInput.txt")(0)
		solveTwo(input)
	}

	def solveTwo(danceMoves: Array[String]): Unit = {
		val positions = ('a' to 'p').toArray
		//val newPositions = solveOneStep(positions, danceMoves).mkString("") 
		//val positionChangeArray = positions.map(newPositions.indexOf(_)).toArray
		//val finalPositions = solveTwoStep(positions, positionChangeArray, 2)
		//println("Final positions " + finalPositions.mkString(","))
		solveTwoStep(positions, danceMoves, 5000000)
	}

	@annotation.tailrec
	def solveTwoStep(positions: Array[Char], danceMoves: Array[String], counter: Int): Array[Char] = {
		if(counter == 0) return positions
		if(counter % 10 == 0) println("Counter " + counter)
		val newPositions = solveOneStep(positions, danceMoves)
		solveTwoStep(newPositions, danceMoves, counter - 1)
	}

	def solveTwoStep(positions: Array[Char], positionChangeArray: Array[Int], counter: Int): Array[Char] = {
		if(counter == 0) return positions
		val newPositions = rearrange(positions, positionChangeArray)
		println("New positions " + newPositions.mkString(","))
		solveTwoStep(newPositions, positionChangeArray, counter - 1)
	}

	def rearrange(positions: Array[Char], positionChangeArray: Array[Int]): Array[Char] = {
		val newPositions = new Array[Char](positions.length)
		(0 to positions.length - 1).foreach(i => newPositions(i) = positions(positionChangeArray(i)))
		newPositions	
	}

	@annotation.tailrec
	def solveOneStep(position: Array[Char], danceMoves: Array[String]): Array[Char] = {
		if(danceMoves.length <= 0) return position
		val danceMove = danceMoves(0)

		val newPosition = if(danceMove.charAt(0) == 's') {
			val spinSize = (danceMove.tail.toString.toInt % position.length)
			val positionFirstHalf = position.slice(0, position.length - spinSize)
			val positionSecondHalf = position.slice(position.length - spinSize, position.length)
			positionSecondHalf ++ positionFirstHalf
		} else if (danceMove.charAt(0) == 'x') {
			val programIndex = danceMove.slice(1, danceMove.length).split("/").map(_.trim.toInt)
			indexSwap(programIndex(0), programIndex(1), position)
		} else if (danceMove.charAt(0) == 'p') {
			val programCharacters = danceMove.slice(1, danceMove.length).split("/").map(_.trim.charAt(0))
			indexSwap(position.indexOf(programCharacters(0)), position.indexOf(programCharacters(1)), position)
		} else {
			println("Fucked")
			position
		}

		//println()
		//println("New position " + newPosition.mkString(","))

		solveOneStep(newPosition, danceMoves.tail)
	}

	def indexSwap(indxOne: Int, indxTwo: Int, position: Array[Char]): Array[Char] = {
		val temp = position(indxOne)
		position(indxOne) = position(indxTwo)
		position(indxTwo) = temp
		position
	}

}
