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
		val startingPieces = bridgePieces.filter(piece => piece.endValues.contains(0))

		val strengths = startingPieces.flatMap(piece => {
			// Create a version without current bridge piece in
			val tempBridgePieces = bridgePieces.clone
			tempBridgePieces -= piece

			//println()
			//println("-------------------------------------")
			//println()
			//println("Chain")

			piece.setTopValue(0)
			piece.setBottomValueFromTopValue()
			solveOneStep(piece, tempBridgePieces, piece.strength)

			//piece.setTopValue(endValues(1))
			//piece.setBottomValueFromTopValue()
			//solveOneStep(piece, tempBridgePieces, piece.strength)
		})

		strengths.foreach(x => println(x))
		println("Strength " + strengths.max)
	}

	def solveOneStep(currentPiece: BridgePiece, bridgePieces: ArrayBuffer[BridgePiece], strength: Int): ArrayBuffer[Int] = {
	
		//println()
		//println("Value")
		//currentPiece.print()		

		val bottomValue = currentPiece.getBottomValue()	
		val matchingBridgePieces = bridgePieces.filter(piece => piece.endValues.contains(bottomValue))
		
		if (matchingBridgePieces.length == 0) {
			println(strength)
			return ArrayBuffer(strength) 
		}

		matchingBridgePieces.flatMap(piece => {		
			val tempBridgePieces = bridgePieces.clone
			tempBridgePieces -= piece

			piece.setTopValue(bottomValue)
			piece.setBottomValueFromTopValue()
			solveOneStep(piece, tempBridgePieces, strength + piece.strength)
		})
	}

	def findMaxWeight(startingPieces: ArrayBuffer[BridgePiece]): Unit = {

	}
}
