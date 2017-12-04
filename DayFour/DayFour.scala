import Utils._
import scala.collection.mutable.ArrayBuffer

object DayFour {

	def main(args: Array[String]) {
		val input = Utils.readIn("DayFourInput.txt");
		println("Word valid " + checkDuplicates(input));
		println("Anagram valid " + checkDuplicatesAnagrams(input));
	}

	def checkDuplicates(input: ArrayBuffer[Array[String]]): Int = {
		input.map(row => {
			if(row.distinct.length == row.length) 1
			else 0
		}).sum
	}

	def checkDuplicatesAnagrams(input: ArrayBuffer[Array[String]]): Int = {
		input.map(row => {
			val rowHashMaps = row.map(word =>
				word.groupBy(_.toChar).mapValues(_.size)			
			)
			if(rowHashMaps.distinct.length == rowHashMaps.length) 1
			else 0
		}).sum
	}

}
