import Utils._
import DayTen._

import scala.collection.mutable.ArrayBuffer

object DayFourteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTenTestInput.txt")(0)
		println(input.mkString(", "))
		val asciiArray = convertToAscii(input)
		println(asciiArray.mkString(", "))
		DayTen.solveTwo(asciiArray, 255)
		//println(convertToAscii(input).mkString(", "))
		//solveOne(input)
	}

	def convertToAscii(input: String): Array[Int] = {
		input.map(ch => ch.toInt).toArray
	}

	def solveOne(input: String): Unit = {
		val counter = 0
		val newValue = input + "-" + counter
		
	}

}
