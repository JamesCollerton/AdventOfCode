import Utils._
import BridgePiece._

import scala.collection.mutable.ArrayBuffer

object DayTwentyFour {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyFourInput.txt")
		input.foreach(_.print())
		solveOne(input)
	}	

	def solveOne(bridgePieces: ArrayBuffer[BridgePiece]): Unit = {
		val startingPieces = makeTree(bridgePieces)
		startingPieces.foreach(piece => {
			printTree(piece, 0)
			println()
		})
		findMaxWeight(startingPieces)
	}

	def findMaxWeight(startingPieces: ArrayBuffer[BridgePiece]): Unit = {
		val maxStrength = startingPieces.flatMap(piece => stepUpTree(piece, piece.strength)).max
		println("Max Strength " + maxStrength)
	}

	def stepUpTree(currentPiece: BridgePiece, runningSum: Int): ArrayBuffer[Int] = {
		if(currentPiece.getNextBridgePieces().length == 0) {
			return ArrayBuffer(runningSum)
		}

		currentPiece.getNextBridgePieces().flatMap(piece => {
			stepUpTree(piece, runningSum + piece.strength)
		})
	}

	def makeTree(bridgePieces: ArrayBuffer[BridgePiece]): ArrayBuffer[BridgePiece] = {
		// Break condition
		if(bridgePieces.length <= 0) return new ArrayBuffer[BridgePiece]

		// Search for a bridge piece with the lowest end value
		val startingPiece = findStartingPiece(bridgePieces)

		// Remove from array buffer
		bridgePieces -= startingPiece

		// Make the tree from that start point
		val startingPieceClone = startingPiece.copy()
		val bridgePiecesClone = bridgePieces.map(piece => piece.copy())
		makeBranches(startingPieceClone, bridgePiecesClone)

		// Repeat
		makeTree(bridgePiecesClone) :+ startingPieceClone
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
			bridgePieces -= bridgePiece
			makeBranches(bridgePiece, bridgePieces)
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
		val minVal = allVals.minBy(x => x._2)._1
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
