import Utils._
import ThreeDVector._

import scala.collection.mutable.ArrayBuffer

object DayTwenty {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyInput.txt")
		solveOne(input)
	}

	def solveOne(propertiesVectors: ArrayBuffer[PropertiesVector]): Unit = {
	
		// Index of particle with lowest acceleration. Fluke because there's only one
		// particle with min acceleration.	
		println("Min acceleration " + propertiesVectors.map(_.getAcceleration()).zipWithIndex.min._2)

	}

}
