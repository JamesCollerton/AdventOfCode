import Utils._

object DayFive {

	def main(args: Array[String]): Unit = {
		// Read in and grab first line
		val inputArray = Utils.readIn("DayFiveInput.txt")(0)
		solveOne(inputArray)
	}

	def solveOne(arr: Array[Int]): Unit = {
		System.out.println("Number of steps " + moveForOne(arr, 0, 0))
	}

	def moveForOne(arr: Array[Int], index: Int, counter: Int): Int = {
		if (index >= arr.length || index < 0) return counter 
		arr(index) += 1
		return moveForOne(arr, index + arr(index) - 1, counter + 1)
	}
	
}
