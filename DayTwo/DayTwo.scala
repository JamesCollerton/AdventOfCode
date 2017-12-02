import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import Utils._

object DayTwo {
	
	def calculateChecksum(rows: ArrayBuffer[Array[Int]]): Int = {
		if(rows.size == 0) return 0
		val currRow = rows(0)
		var divided = currRow(0)
		var divisor = currRow(0)
		for(i <- currRow){
			for(j <- currRow){
				if(i % j == 0 && i != j){
					divided = i;
					divisor = j;	
				}
			}
		}
		println("Row " + currRow.mkString(" "))
		println("Divided " + divided)
		println("Divisor " + divisor)
		println()
		calculateChecksum(rows.slice(1, rows.length)) + divided / divisor
	}

	def main(args: Array[String]): Unit = {
		val rows = Utils.readIn("DayTwoInput.txt");
		println("Checksum " + calculateChecksum(rows));
	}	

}
