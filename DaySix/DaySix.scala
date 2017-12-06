import Utils._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object DaySix {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DaySixInput.txt")(0)
		//solveOne(input)
		solveTwo(input)
	}

	def solveOne(arr: Array[Int]): Unit = {
		println("Number of moves " + moveOne(arr, ArrayBuffer(), 0))
	}

	def solveTwo(arr: Array[Int]): Unit = {
		println("Number of moves " + moveTwo(arr, ArrayBuffer(), 0))
	}

	@tailrec
	def moveOne(arr: Array[Int], doneArr: ArrayBuffer[Array[Int]], counter: Int): Int = {

		doneArr += arr.clone

		var maxIndex = arr.zipWithIndex.maxBy(_._1)._2
		var maxAmount = arr(maxIndex)
		arr(maxIndex) = 0

		while(maxAmount > 0) {
			maxIndex += 1
			if (maxIndex >= arr.length) maxIndex = 0
			arr(maxIndex) += 1
			maxAmount -= 1	
		}
	
		if (doneArr.map(finArr => finArr.sameElements(arr)).contains(true)) return counter + 1
		moveOne(arr, doneArr, counter + 1)
	}

	@tailrec
	def recalculationStep(arr: Array[Int], maxIndex: Int, maxAmount: Int): Unit = {
		if (maxAmount <= 0) return
		if (maxIndex >= arr.length){  
			arr(0) += 1
			recalculationStep(arr, 1, maxAmount -1)
		} else {
			arr(maxIndex) += 1
			recalculationStep(arr, maxIndex + 1, maxAmount -1)
		}
	}

	def recalculateArray(arr: Array[Int]): Unit = {

		val maxIndex = arr.zipWithIndex.maxBy(_._1)._2
		val maxAmount = arr(maxIndex)
		arr(maxIndex) = 0

		recalculationStep(arr, maxIndex + 1, maxAmount)

	}

	@tailrec
	def moveTwo(arr: Array[Int], doneArr: ArrayBuffer[Array[Int]], counter: Int): Int = {

		doneArr += arr.clone

		recalculateArray(arr)	

		val matchingIndex = doneArr.map(finArr => finArr.sameElements(arr)).indexOf(true)

		if (matchingIndex > -1) return counter + 1 - matchingIndex
		moveTwo(arr, doneArr, counter + 1)
	
	}


}
