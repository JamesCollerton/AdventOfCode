import scala.io.Source;

object DayTwo {

	def readIn(): Unit = {

		val rows = ArrayBuffer[Array[String]]()

		def printTest(resource: Source): ArrayBuffer[Array[String]] = {
			for (line <- resource.getLines) {
				rows += line.split("\t").map(_.trim);
//				println(line)
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
