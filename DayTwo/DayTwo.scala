import scala.io.Source;

object DayTwo {

	def readIn(): Unit = {
		val bufferedSource = Source.fromFile("DayTwoInput.txt");

		for (line <- bufferedSource.getLines) {
			println(line)
		}

		bufferedSource.close;

//		for (line <- bufferedSource.getLines) {
//			val cols = line.split("\t").map(_.trim)
//			// do whatever you want with the columns here
//			println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")
//		}
	}

	def calculateChecksum(): Unit = {
	}

	def main(args: Array[String]): Unit = {
		readIn();
	}	

}
