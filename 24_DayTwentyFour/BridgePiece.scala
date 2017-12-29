package BridgePiece

import scala.collection.mutable.ArrayBuffer

class BridgePiece(val endValues: Array[Int]) {

	val strength = endValues.sum
	
	private var topValue = 0;
	private var bottomValue = 0;
	private var nextBridgePieces = new ArrayBuffer[BridgePiece];

	def setTopValue(topValue: Int): Unit = {
		this.topValue = topValue
	}

	def setBottomValue(bottomValue: Int): Unit = {
		this.bottomValue = bottomValue
	}

	def setBottomValueFromTopValue(): Unit = {
		val notTopValue = endValues.filter(_ != topValue)
		this.bottomValue = if (notTopValue.length == 0) topValue else notTopValue(0)
	}

	def getTopValue(): Int = {
		topValue
	}

	def getBottomValue(): Int = {
		bottomValue
	}

	def addNextBridgePiece(nextBridgePiece: BridgePiece): Unit = {
		nextBridgePieces += nextBridgePiece
	}

	def setNextBridgePieces(nextBridgePieces: ArrayBuffer[BridgePiece]): Unit = {
		this.nextBridgePieces = nextBridgePieces
	}

	def getNextBridgePieces(): ArrayBuffer[BridgePiece] = {
		nextBridgePieces
	}

	def print(): Unit = {
		println("Values: " + endValues(0) + "/" + endValues(1))
		println("Top value: " + topValue)
		println("Bottom value: " + bottomValue)
	}

	def copy(): BridgePiece = {
		val newPiece = new BridgePiece(this.endValues)
		newPiece.setTopValue(this.topValue)
		newPiece.setBottomValue(this.bottomValue)
		newPiece.setNextBridgePieces(this.nextBridgePieces)
		newPiece
	}

}
