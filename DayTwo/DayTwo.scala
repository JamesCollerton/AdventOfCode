import scala.io.Source;

object DayTwo {

	def readIn(): Unit = {

		def printTest(resource: Source): Unit = {
			for (line <- resource.getLines) {
				println(line)
			}
		}

		def using[A <: { def close(): Unit }, B](resource: A)(f: A => B){
			try {
				f(resource)
			} finally {
				resource.close;
			}
		}

		using(Source.fromFile("DayTwoInput.txt"))(printTest);		

	}
	
	def calculateChecksum(): Unit = {
	}

	def main(args: Array[String]): Unit = {
		readIn();
	}	

}
