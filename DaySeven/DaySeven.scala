import Utils._

import scala.collection.mutable.HashMap

object DaySeven {

	def main(args: Array[String]): Unit = {
		val programs = Utils.readIn("DaySevenInput.txt")
		val bottom = findBottom(programs)
                println("Bottom is " + bottom)
                findWeights(programs, bottom)
	}

	def findBottom(programs: HashMap[String, Array[String]]): String = {

		// Find all supporting ones
		val supportingPrograms = programs.filter(prog => prog._2.length > 0).keySet.toArray.map(name => name.split(" ")(0))

		// Find all supported ones
		val supportedPrograms = programs.filter(prog => prog._2.length > 0).map(prog => prog._2).flatten

		// The one that is supporting but is not supported is bottom
		supportingPrograms.toSet.filterNot(supportedPrograms.toSet).head.toString

	}

        def findWeights(programs: HashMap[String, Array[String]], bottom: String): Unit = {
    
                // Create HashMap of individual weights
                val programWeights: HashMap[String, Int] = programs.map{ 
                    case (k, v) => {
                        (k.split(" ")(0), k.split(" ")(1).filter(!"()".contains(_)).toInt)
                    }
                }

                // Create HashMap of branches
                val programBranches: HashMap[String, Array[String]] = programs.map{ 
                    case (k, v) => (k.split(" ")(0), v)
                }
                
                //println("Results for " + programBranches(bottom).map(prog => recurseStep(prog, programWeights, programBranches)).mkString(" "))
                val bottomBranch = programBranches(bottom).map(prog => (prog, programWeights(prog), recurseStep(prog, programWeights, programBranches)))
                val majorityValue = bottomBranch.map(x => x._3).groupBy(identity).maxBy(_._2.size)._1
                val oddBranch = bottomBranch.filter(x => x._3 != majorityValue) //.map(x => x._2 + (majorityValue - x._3))
                println(oddBranch(0)._1 + " " + oddBranch(0)._2  + " " + oddBranch(0)._3)
                //println("New value " + oddBranch.mkString(""))

        }

        def recurseStep(bottom: String, programWeights: HashMap[String, Int], programBranches: HashMap[String, Array[String]]): Int = {
              programBranches(bottom).map(prog => recurseStep(prog, programWeights, programBranches)).sum + programWeights(bottom)
        }

}
