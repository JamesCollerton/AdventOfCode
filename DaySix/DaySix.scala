import Utils._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object DaySix {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DaySixInput.txt")(0)
		solveOne(input)
	}

	def solveOne(arr: Array[Int]): Unit = {
		println("Number of moves " + moveOne(arr, ArrayBuffer(), 0))
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

//		println()
//		doneArr.foreach(finArr => println(finArr.mkString(" ")))
//		println()
//		println(arr.mkString(" "))
	
		if (doneArr.map(finArr => finArr.sameElements(arr)).contains(true)) return counter + 1
		moveOne(arr, doneArr, counter + 1)
	}

}
