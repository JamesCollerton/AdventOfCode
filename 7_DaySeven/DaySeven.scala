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
                
		recurseStep(bottom, programWeights, programBranches)

        }

        def recurseStep(bottom: String, programWeights: HashMap[String, Int], programBranches: HashMap[String, Array[String]]): (String, Int) = {
                
		val branchLeaves = programBranches(bottom).map(prog => recurseStep(prog, programWeights, programBranches))

		if(branchLeaves.length > 0) {
			val majorityValue = branchLeaves.map(x => x._2).groupBy(identity).maxBy(_._2.size)._1
			val oddBranch = branchLeaves.filter(x => x._2 != majorityValue)
	
			if(oddBranch.size != 0) {
				println()
				println("Branch value " + oddBranch(0)._1)
				println("Branch weight " + programWeights(oddBranch(0)._1))
				println("Weight we're aiming for " + majorityValue)
				println("Weight we're at " + oddBranch(0)._2)
				println("New branch value " + (programWeights(oddBranch(0)._1) + (majorityValue - oddBranch(0)._2)))
			} 

			(bottom, branchLeaves.map(x => x._2).sum + programWeights(bottom))
		} else {
			(bottom, programWeights(bottom))
		}

        }

}
