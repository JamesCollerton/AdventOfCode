
object DayFifteen {

	def main(args: Array[String]): Unit = {
		solveOne();
	}

	def solveOne(): Unit = {
		println("Number of matches " + solveOneStep(699, 124, 40000000, 0))
	}

	// Note, 2147483647 is the maximum value you can store in an Int
	// so you need to cast up to double to stop overflow.
	@annotation.tailrec
	def solveOneStep(aValue: Int, bValue: Int, remaining: Int, counter: Int): Int = {
		if(remaining <= 0) return counter
		val divisor = 2147483647
		val aRemainder = ((aValue.toDouble * 16807) % divisor.toDouble).toInt
		val bRemainder = ((bValue.toDouble * 48271) % divisor.toDouble).toInt
		val aBinary = aRemainder.toBinaryString.takeRight(16)
		val bBinary = bRemainder.toBinaryString.takeRight(16)
		val countIncrement = if(aBinary == bBinary){ 1 } else { 0 }
		solveOneStep(aRemainder.toInt, bRemainder.toInt, remaining - 1, counter + countIncrement)
	}

}
