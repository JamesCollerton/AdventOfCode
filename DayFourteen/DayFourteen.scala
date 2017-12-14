import Utils._
import DayTen._

import scala.collection.mutable.ArrayBuffer

object DayFourteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayFourteenInput.txt")(0)
		solveOne(input)
	}

	def solveOne(input: String): Unit = {
		val hashGrid = (0 to 127).map(i => {
			val knotHash = calculateKnotHash(input + "-" + i)
			knotHash.map(ch => pad(Integer.parseInt(ch.toString, 16).toBinaryString)).mkString("")
		})
		println("Sum " + hashGrid.map(_.count(_ == '1')).sum)		
	}

	def pad(input: String): String = {
		("0000" + input).takeRight(4)
	}

	def calculateKnotHash(toBeHashed: String): String = {
		val asciiArray = toBeHashed.map(ch => ch.toInt).toArray
		DayTen.solveTwo(asciiArray, 255)
	}

}
