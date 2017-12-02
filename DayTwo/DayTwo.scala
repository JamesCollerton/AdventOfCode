import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import Utils._

object DayTwo {
	
	def calculateChecksum(rows: ArrayBuffer[Array[Int]]): Int = {
		if(rows.size == 0) return 0
		val currRow = rows(0)
		for (
			i <- currRow
			j <- currRow
		) {

		}
		for(i <- currRow){
			for(j <- currRow){
				if(i % j == 0 && i != j){
				return calculateChecksum(rows.slice(1, rows.length)) + i / j
				}
			}
		}
	}

	def main(args: Array[String]): Unit = {
		// Answer is 233
		val rows = Utils.readIn("DayTwoInput.txt");
		println("Checksum " + calculateChecksum(rows));
	}	

}
