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

	def print(): Unit = {
		println("Values: " + endValues(0) + "/" + endValues(1))
		println("Top value: " + topValue)
		println("Bottom value: " + bottomValue)
	}	

}
