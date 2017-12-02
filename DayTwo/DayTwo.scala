import scala.io.Source;

object DayTwo {

	def readIn(): Unit = {

		val bufferedSource;

		try {

			bufferedSource = Source.fromFile("DayTwoInput.txt");

			for (line <- bufferedSource.getLines) {
				println(line)
			}

		} finally {
			bufferedSource.close;
		}

	}
	
	def calculateChecksum(): Unit = {
	}

	def main(args: Array[String]): Unit = {
		readIn();
	}	

}
