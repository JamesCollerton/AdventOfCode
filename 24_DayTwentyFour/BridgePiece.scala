package BridgePiece

class BridgePiece(val endValues: Array[Int]) {

	val strength = endValues.sum
	
	private var topValue = 0;
	private var bottomValue = 0;
	private var nextBridgePiece = this;

	def setTopValue(topValue: Int): Unit = {
		this.topValue = topValue
	}

	def setBottomValue(bottomValue: Int): Unit = {
		this.bottomValue = bottomValue
	}

	def getTopValue(): Int = {
		topValue
	}

	def getBottomValue(): Int = {
		bottomValue
	}

	def setNextBridgePiece(nextBridgePiece: BridgePiece): Unit = {
		this.nextBridgePiece = nextBridgePiece
	}

	def print(): Unit = {
		println(endValues(0) + "/" + endValues(1))
	}	

}
