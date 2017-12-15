import scala.collection.mutable.ArrayBuffer

object DayFifteen {

	def main(args: Array[String]): Unit = {
		solveTwo();
	}

	def solveTwo(): Unit = {
		val aList = generateList(65,   5000000, 16807, 4, ArrayBuffer())
		val bList = generateList(8921, 5000000, 48271, 8, ArrayBuffer())

		println("Got lists")
		println("A " + aList.length + " B " + bList.length)

		val clashingIndexes = for {
			i <- 0 to Math.min(aList.length, bList.length) - 1
			if (aList(i).toBinaryString.takeRight(16) == bList(i).toBinaryString.takeRight(16))
		} yield 1

		println("Sum " + clashingIndexes.sum)
	}

	@annotation.tailrec
	def generateList(value: Int, remaining: Int, multiplier: Int, factor: Int, array: ArrayBuffer[Int]): ArrayBuffer[Int] = {
		if(array.length > remaining) return array
		val divisor = 2147483647
		val remainder = ((value.toDouble * multiplier.toDouble) % divisor.toDouble).toInt
		if (remainder % factor == 0) array += remainder
		generateList(remainder, remaining, multiplier, factor, array)
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
