import Utils._
import scala.collection.mutable.ArrayBuffer

object DayFour {

	def main(args: Array[String]) {
		val input = Utils.readIn("DayFourInput.txt");
		println(checkDuplicates(input));
	}

	def checkDuplicates(input: ArrayBuffer[Array[String]]): Int = {
		input.map(row => {
			if(row.distinct.length == row.length) 1
			else 0
		}).sum
	}

}
