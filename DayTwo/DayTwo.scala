import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object DayTwo {

	def readIn(): Unit = {

		def readRows(resource: Source): ArrayBuffer[Array[String]] = {
			val rows = ArrayBuffer[Array[String]]()

			for (line <- resource.getLines) {
				rows += line.split("\t").map(_.trim)
//				println(line)
			}

			printRows(rows)

			rows
		}

		def printRows(rows : ArrayBuffer[Array[String]]): Unit = {
			for(fieldRow <- rows){
				println(fieldRow.mkString(" "));
			}
//				for(field <- fieldRow){
//					print(field)
//				}	
//				println()
//			}
		}

		def using[A <: { def close(): Unit }, B](resource: A)(f: A => B){
			try {
				f(resource)
			} finally {
				resource.close;
			}
		}

		using(Source.fromFile("DayTwoInput.txt"))(readRows);		

	}
	
	def calculateChecksum(): Unit = {
	}

	def main(args: Array[String]): Unit = {
		readIn();
	}	

}
