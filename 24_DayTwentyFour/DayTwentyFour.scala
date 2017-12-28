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

		// Remove from array buffer
		bridgePieces -= startingPiece

		// Search for all bridge pieces with an end value of the other end's value
		// Add all of them to the tree
		// Remove them from the arraybuffer
		makeBranches(startingPiece, bridgePieces)

		// Print tree
		// Check remaining bridge pieces
		printTree(startingPiece, 0)
		println()
		bridgePieces.foreach(_.print())

		// Repeat

		// Will return an array buffer of all of the starting nodes for the tree

	}

	def printTree(startingPiece: BridgePiece, counter: Int): Unit = {
		println()
		println("Level " + counter)
		startingPiece.print()
		startingPiece.getNextBridgePieces().foreach(bridgePiece => {
			printTree(bridgePiece, counter + 1)
		})
	}

	def makeBranches(currentPiece: BridgePiece, bridgePieces: ArrayBuffer[BridgePiece]): Unit = {
		
		println()
		println("--------------------------------------------")
		println()
		println("Finding All Branches for Current Piece")	
		currentPiece.print()
		
		// All indexes for each end value
		val allEndValues = bridgePieces.zipWithIndex.flatMap{ case (bp, idx) => bp.endValues.map(ev => (idx, ev)) }
		
		// Indexes of each of the next pieces matching the bottom value
		val nextPiecesIndexes = allEndValues.filter(x => x._2 == currentPiece.getBottomValue()).map(_._1).distinct

		// All of the next pieces by index
		val nextPieces = nextPiecesIndexes.collect(bridgePieces)		
		
		println("Next piece indexes")
		nextPiecesIndexes.foreach(x => println(x))
		println("Next pieces")
		nextPieces.foreach(_.print())

		nextPieces.foreach(bridgePiece => {
			// Set top values	
			bridgePiece.setTopValue(currentPiece.getBottomValue())

			// Set bottom values
			bridgePiece.setBottomValueFromTopValue()

			// Make next branch
			currentPiece.addNextBridgePiece(bridgePiece)

			// Remove from array buffer and recurse up
			val tempBridgePieces = bridgePieces.clone
			//tempBridgePieces -= bridgePiece
			bridgePieces -= bridgePiece
			makeBranches(bridgePiece, bridgePieces)

			// Remove permanently from array buffer
			bridgePieces -= bridgePiece
		})

		println()
		println("--------------------------------------------")
		println()

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
		minPiece.setBottomValueFromTopValue()

		minPiece.print()

		println()
		println("--------------------------------------------")
		println()

		minPiece
	}

}
