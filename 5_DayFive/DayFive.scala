import Utils._

import scala.annotation.tailrec

object DayFive {

	def main(args: Array[String]): Unit = {
		// Read in and grab first line
		val inputArray = Utils.readIn("DayFiveInput.txt")(0)
		//solveOne(inputArray)
		solveTwo(inputArray)
	}

	def solveOne(arr: Array[Int]): Unit = {
		println("Number of steps " + moveForOne(arr, 0, 0))
	}

	def solveTwo(arr: Array[Int]): Unit = {
		println("Number of steps " + moveForTwo(arr, 0, 0))
	}

	def moveForOne(arr: Array[Int], index: Int, counter: Int): Int = {
		if (index >= arr.length || index < 0) return counter 
		arr(index) += 1
		return moveForOne(arr, index + arr(index) - 1, counter + 1)
	}

	@tailrec
	def moveForTwo(arr: Array[Int], index: Int, counter: Int): Int = {
		if (index >= arr.length || index < 0){
			println(arr.mkString(" "))
			return counter 
		}
		val incr = if (arr(index) >= 3) -1 else 1
		arr(index) += incr
		moveForTwo(arr, index + arr(index) - incr, counter + 1)
	}
	
}
