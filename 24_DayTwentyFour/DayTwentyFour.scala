import Utils._
import BridgePiece._

import scala.collection.mutable.ArrayBuffer

object DayTwentyFour {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyFourInput.txt")
		solveOne(input)
	}	

	def solveOne(bridgePieces: ArrayBuffer[BridgePiece]): Unit = {
		val startingPieces = bridgePieces.filter(piece => piece.endValues.contains(0))

		val strengths = startingPieces.flatMap(piece => {
			// Create a version without current bridge piece in
			val tempBridgePieces = bridgePieces.clone
			tempBridgePieces -= piece

			piece.setTopValue(0)
			piece.setBottomValueFromTopValue()
			solveOneStep(piece, tempBridgePieces, piece.strength, 1)
		})

		println("Strength " + strengths.maxBy(_._2))
	}

	def solveOneStep(currentPiece: BridgePiece, bridgePieces: ArrayBuffer[BridgePiece], strength: Int, length: Int): ArrayBuffer[(Int, Int)] = {

		val bottomValue = currentPiece.getBottomValue()	
		val matchingBridgePieces = bridgePieces.filter(piece => piece.endValues.contains(bottomValue))
		
		if (matchingBridgePieces.length == 0) {
			return ArrayBuffer((strength, length)) 
		}

		matchingBridgePieces.flatMap(piece => {		
			val tempBridgePieces = bridgePieces.clone
			tempBridgePieces -= piece

			piece.setTopValue(bottomValue)
			piece.setBottomValueFromTopValue()
			solveOneStep(piece, tempBridgePieces, strength + piece.strength, length + 1)
		})
	}

}
