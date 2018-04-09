import scala.collection.mutable.ArrayBuffer

object DayFifteen {

	def main(args: Array[String]): Unit = {
		solveTwo();
	}

	def solveTwo(): Unit = {
		println("Num matches " + solveTwoStep(699, 124, 0, 0))
	}

	@annotation.tailrec
	def solveTwoStep(aValue: Int, bValue: Int, counter: Int, numPairs: Int): Int = {
		if(counter == 5000000) return numPairs
		val aNewValue = generateNextValue(aValue, 16807, 4, true)
		val bNewValue = generateNextValue(bValue, 48271, 8, true)
		val newNumPairs = if(aNewValue.toBinaryString.takeRight(16) == bNewValue.toBinaryString.takeRight(16)) {
			numPairs + 1	
		} else {
			numPairs
		}
		solveTwoStep(aNewValue, bNewValue, counter + 1, newNumPairs) 
	}

	@annotation.tailrec
	def generateNextValue(currValue: Int, multiplier: Int, factor: Int, firstPass: Boolean): Int = {
		if(!firstPass && currValue % factor == 0) return currValue
		val divisor = 2147483647
		val remainder = ((currValue.toDouble * multiplier.toDouble) % divisor.toDouble).toInt
		generateNextValue(remainder, multiplier, factor, false)	
	}

}
