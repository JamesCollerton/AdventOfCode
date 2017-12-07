import Utils._

import scala.collection.mutable.HashMap

object DaySeven {

	def main(args: Array[String]): Unit = {
		val programs = Utils.readIn("DaySevenTestInput.txt")
		// findBottom(programs)
                findWeights(programs)
	}

	def findBottom(programs: HashMap[String, Array[String]]): Unit = {

		// Find all supporting ones
		val supportingPrograms = programs.filter(prog => prog._2.length > 0).keySet.toArray.map(name => name.split(" ")(0))

		// Find all supported ones
		val supportedPrograms = programs.filter(prog => prog._2.length > 0).map(prog => prog._2).flatten

		// The one that is supporting but is not supported is bottom
		println("Bottom is " + (supportingPrograms.toSet.filterNot(supportedPrograms.toSet)))

	}

        def findWeights(programs: HashMap[String, Array[String]]): Unit = {
    
                // Create HashMap of individual weights
                val programWeights: HashMap[String, Int] = programs.map{ 
                    case (k, v) => {
                        (k.split(" ")(0), k.split(" ")(1).filter(!"()".contains(_)).toInt)
                    }
                }

                programWeights.foreach(x => println(x._1 + " -> " + x._2))

                // Create HashMap of branches
//                val programBranches: HashMap[String, Array[String]] =

        }

}
