import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import Utils._

object DayTwo {
	
	def calculateChecksum(rows: ArrayBuffer[Array[Int]]): Int = {
		rows.map(row => {
			val divisions = for {
						i <- row
						j <- row if i != j
						d <- i % j match {
							case 0 => Some(i / j)
							case _ => None
						}  
					} yield d
			divisions.sum
			}
		).sum
	}		

	def main(args: Array[String]): Unit = {
		// Answer is 233
		val rows = Utils.readIn("DayTwoInput.txt");
		println("Checksum " + calculateChecksum(rows));
	}	

}
