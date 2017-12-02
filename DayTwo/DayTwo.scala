import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object DayTwo {

	def readIn(): ArrayBuffer[Array[String]] = {

		def readRows(resource: Source): ArrayBuffer[Array[String]] = {
			val rows = ArrayBuffer[Array[String]]()

			for (line <- resource.getLines) {
				rows += line.split("\t").map(_.trim)
			}

			//printRows(rows)

			rows
		}

		def printRows(rows : ArrayBuffer[Array[String]]): Unit = {
			for(fieldRow <- rows){
				println(fieldRow.mkString(" "));
			}
		}

		def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B = {
			try {
				f(resource)
			} finally {
				resource.close;
			}
		}

		using(Source.fromFile("DayTwoInput.txt"))(readRows);		

	}
	
	def calculateChecksum(rows: ArrayBuffer[Array[String]]): Int = {
		if(rows.size == 0) return 0;
		val diff = rows(0).max.toInt - rows(0).min.toInt 
		println("Row " + rows(0).mkString(" "))
		println("Max " + rows(0).max.toInt)
		println("Min " + rows(0).min.toInt)
		println("Difference found " + diff)
		println()
		calculateChecksum(rows.slice(1, rows.length)) + diff
	}

	def main(args: Array[String]): Unit = {
		val rows = readIn();
		calculateChecksum(rows);
	}	

}
