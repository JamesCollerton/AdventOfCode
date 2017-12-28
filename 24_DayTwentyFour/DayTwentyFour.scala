import Utils._
import BridgePiece._

import scala.collection.mutable.ArrayBuffer

object DayTwentyFour {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyFourTestInput.txt")
		input.foreach(_.print())
		solveOne(input)
	}	

	def solveOne(bridgePieces: ArrayBuffer[BridgePiece]): Unit = {
		makeTree(bridgePieces)
	}

	def makeTree(bridgePieces: ArrayBuffer[BridgePiece]): Unit = {
		// Search for a bridge piece with the lowest end value
		val startingPiece = findStartingPiece(bridgePieces)

		// Search for all brige pieces with an end value of the other end's value

		// Add all of them to the tree

		// Remove them from the arraybuffer

		// Repeat

		// Will return an array buffer of all of the starting nodes for the tree

	}

	def findStartingPiece(bridgePieces: ArrayBuffer[BridgePiece]): BridgePiece = {
	
		println()
		println("--------------------------------------------")
		println()
		println("Finding Start Piece")	

		// Gets all end values and indexes
		val allVals = bridgePieces.zipWithIndex.flatMap{ case (bp, idx) => bp.endValues.map(ev => (idx, ev)) }

		println("Found all end values and indexes")
		allVals.foreach(v => println(v))

		// Find minimum end value
		val minVal = allVals.min._2
		val minPiece = bridgePieces(minVal)

		println("Found minimum")
		println(minVal)
		minPiece.print()

		minPiece.setTopValue(minPiece.endValues.min)
		minPiece.setBottomValue(minPiece.endValues.max)

		println()
		println("--------------------------------------------")
		println()

		minPiece
	}

}
