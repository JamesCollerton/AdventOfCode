import Utils._

object DayTen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTenInput.txt")(0)
		solveOne(input, 255)
	}

	def solveOne(lengths: Array[Int], endNum: Int): Unit = {
		val seq = (0 to endNum).toArray
		println("Solved one " + solveOneStep(seq, lengths, 0, 0))
	}

	def solveOneStep(arr: Array[Int], lengths: Array[Int], currPos: Int, skip: Int): Int = {

		if(lengths.length <= 0) return arr(0) * arr(1)
		println()

		println("Array " + arr.mkString(" "))
		println("Length " + lengths(0))
		println("Current Position " + currPos)
		println("Skip " + skip)

		// Find position we want to reverse to
		val reversePos = currPos + lengths(0)
		val overEdge = if(reversePos - arr.length > 0) reversePos - arr.length else 0
		println("Reverse position " + reversePos)
		println("Over the edge " + overEdge)

		// Reverse from current position to next position

		if(overEdge > 0) {
			def arrStream: Stream[Int] = arr.toStream #::: arrStream
			val rotatedArr = arrStream.take(arr.length + overEdge).slice(overEdge, arr.length + overEdge).toArray
			println("Rotated array " + rotatedArr.mkString(" "))

			val adjCurrPos = currPos - overEdge
			val reversedArr = rotatedArr.slice(0, adjCurrPos) ++ 
					  rotatedArr.slice(adjCurrPos, reversePos).reverse ++
					  rotatedArr.slice(reversePos, rotatedArr.length)
			println("Reversed array " + reversedArr.mkString(" "))

			val rearrangedArr = reversedArr.slice(reversedArr.length - overEdge, reversedArr.length) ++
					    reversedArr.slice(0, reversedArr.length - overEdge)
			println("Rearranged array " + rearrangedArr.mkString(" "))

			val newCurrPos = (currPos + lengths(0) + skip) % rearrangedArr.length
			solveOneStep(rearrangedArr, lengths.tail, newCurrPos ,skip + 1)
		} else {
			val rearrangedArr = arr.slice(0, currPos) ++ 
					    arr.slice(currPos, reversePos).reverse ++ 
					    arr.slice(reversePos, arr.length)
			println("Rearranged array " + rearrangedArr.mkString(" "))
			
			val newCurrPos = (currPos + lengths(0) + skip) % rearrangedArr.length
			solveOneStep(rearrangedArr, lengths.tail, newCurrPos ,skip + 1)
		}

		// Change current position to length + skip position 
		// Remove first element from lengths
		// Increment the skip position
		// Call again

	} 

}
