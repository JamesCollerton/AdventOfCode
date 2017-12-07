import Utils._

import scala.collection.mutable.HashMap

object DaySeven {

	def main(args: Array[String]): Unit = {
		val programs = Utils.readIn("DaySevenInput.txt")
		findBottom(programs)
	}

	def findBottom(programs: HashMap[String, Array[String]]): Unit = {

		// Find all supporting ones
		val supportingPrograms = programs.filter(prog => prog._2.length > 0).keySet.toArray.map(name => name.split(" ")(0))

		// Find all supported ones
		val supportedPrograms = programs.filter(prog => prog._2.length > 0).map(prog => prog._2).flatten

		// The one that is supporting but is not supported is bottom
		println("Bottom is " + (supportingPrograms.toSet.filterNot(supportedPrograms.toSet)))

	}

	

}
