import Utils._

import scala.collection.mutable.HashMap

object DaySixteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DaySixteenInput.txt")(0)
		solveTwo(input)
	}

	def solveTwo(danceMoves: Array[String]): Unit = {
		val positions = ('a' to 'p').toArray
		println("Solved " + solveTwoStep(positions, danceMoves, Array[Array[Char]]()).mkString(""))
	}

	// We essentially just want to track when it gets into a loop.
	@annotation.tailrec
	def solveTwoStep(positions: Array[Char], danceMoves: Array[String], donePositions: Array[Array[Char]]): Array[Char] = {
		if(donePositions.map(_.mkString("")).contains(positions.mkString(""))) {
			val firstIndex = donePositions.map(_.mkString("")).indexOf(positions.mkString(""))
			val gap = donePositions.length - firstIndex
			val remainingMoves = 1000000000 - donePositions.length
			val positionIndex = remainingMoves % gap
			return donePositions(positionIndex)
		}
		val newDonePositions = donePositions :+ positions.clone
		val newPositions = solveOneStep(positions, danceMoves)
		solveTwoStep(newPositions, danceMoves, newDonePositions)
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
