import Utils._

object DayTen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTenInput.txt")(0)
		solveTwo(input, 255)
	}

	def solveTwo(lengths: Array[Int], endNum: Int): Unit = {

		// Create array
		val seq = (0 to endNum).toArray

		// Convert lengths into ASCII (think this works)
		val byteArr = lengths.flatMap(c => c.toString.map(_.toChar.toInt) :+ ','.toInt).dropRight(1) ++ 
			      List(17, 31, 73, 47, 23)

		// Run the solution 64 times, I think this is
		// just the same as keeping running 64 times.
		val byteArr64 = Stream.continually(byteArr.toStream).flatten.take(64 * byteArr.length).toArray
		val sparseHash = solveOneStep(seq, byteArr64, 0, 0)

		// Fairly confident here down is correct
		val denseHash = (0 to 15).map(i => sparseHash.slice(16 * i, 16 * (i + 1)).reduce(_ ^ _))
		println("Dense Hash " + denseHash.mkString(" "))

		// Convert to hexadecimal
		println(denseHash.map(_.toHexString.reverse.padTo(2, "0").reverse.mkString("")).mkString(""))

	}

	def solveOneStep(arr: Array[Int], lengths: Array[Int], currPos: Int, skip: Int): Array[Int] = {

		if(lengths.length <= 0) return arr

		// Find position we want to reverse to
		val reversePos = currPos + lengths(0)
		val overEdge = if(reversePos - arr.length > 0) reversePos - arr.length else 0

		// Reverse from current position to next position

		if(overEdge > 0) {
			def arrStream: Stream[Int] = arr.toStream #::: arrStream
			val rotatedArr = arrStream.take(arr.length + overEdge).slice(overEdge, arr.length + overEdge).toArray

			val adjCurrPos = currPos - overEdge
			val reversedArr = rotatedArr.slice(0, adjCurrPos) ++ 
					  rotatedArr.slice(adjCurrPos, reversePos).reverse ++
					  rotatedArr.slice(reversePos, rotatedArr.length)

			val rearrangedArr = reversedArr.slice(reversedArr.length - overEdge, reversedArr.length) ++
					    reversedArr.slice(0, reversedArr.length - overEdge)

			val newCurrPos = (currPos + lengths(0) + skip) % rearrangedArr.length
			solveOneStep(rearrangedArr, lengths.tail, newCurrPos ,skip + 1)
		} else {
			val rearrangedArr = arr.slice(0, currPos) ++ 
					    arr.slice(currPos, reversePos).reverse ++ 
					    arr.slice(reversePos, arr.length)
			
			val newCurrPos = (currPos + lengths(0) + skip) % rearrangedArr.length
			solveOneStep(rearrangedArr, lengths.tail, newCurrPos ,skip + 1)
		}

	} 

}
