import Utils._

import scala.collection.mutable.HashMap

object DaySixteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DaySixteenInput.txt")(0)
		solveOne(input)
	}

	def solveOne(danceMoves: Array[String]): Unit = {
		val positions = ('a' to 'p').toArray	
		println("New position " + solveOneStep(positions, danceMoves).mkString(""))
	}

	def solveOneStep(position: Array[Char], danceMoves: Array[String]): Array[Char] = {
		if(danceMoves.length <= 0) return position
		val danceMove = danceMoves(0)

		val newPosition = if(danceMove.charAt(0) == 's') {
			val spinSize = (danceMove.tail.toString.toInt % position.length)
			val positionFirstHalf = position.slice(0, position.length - spinSize)
			val positionSecondHalf = position.slice(position.length - spinSize, position.length)
			println()
			println("Raw Spin Size " + danceMove.tail) 
			println("Mod Spin Size " + spinSize)
			println(position.mkString(","))
			println(positionFirstHalf.mkString(","))
			println(positionSecondHalf.mkString(","))
			println("Result " + (positionSecondHalf ++ positionFirstHalf).mkString(","))
			positionSecondHalf ++ positionFirstHalf
		} else if (danceMove.charAt(0) == 'x') {
		//	println("Swapping by Index")
			val programIndex = danceMove.slice(1, danceMove.length).split("/").map(_.trim.toInt)
			indexSwap(programIndex(0), programIndex(1), position)
		} else if (danceMove.charAt(0) == 'p') {
		//	println("Swapping by Character")
			val programCharacters = danceMove.slice(1, danceMove.length).split("/").map(_.trim.charAt(0))
			indexSwap(position.indexOf(programCharacters(0)), position.indexOf(programCharacters(1)), position)
		} else {
			println("Fucked")
			position
		}

		println("New position " + newPosition.mkString(","))
		println()

		solveOneStep(newPosition, danceMoves.tail)
	}

	def indexSwap(indxOne: Int, indxTwo: Int, position: Array[Char]): Array[Char] = {
		val temp = position(indxOne)
		position(indxOne) = position(indxTwo)
		position(indxTwo) = temp
		position
	}

}
