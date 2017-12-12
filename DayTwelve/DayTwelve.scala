import Utils._

import scala.collection.mutable.HashMap

object DayTwelve {

	def main(args: Array[String]) = {
		val input = Utils.readIn("DayTwelveInput.txt")
		solveOne(input)	
	}

	def solveOne(programs: HashMap[Int, Array[Int]]): Unit = {
		println("Number in group " + solveOneStep(0, programs, new Array(0)).length)
	}

	def solveOneStep(currProg: Int, programs: HashMap[Int, Array[Int]], visitedPrograms: Array[Int]): Array[Int] = {
		if (visitedPrograms.contains(currProg)) return visitedPrograms
		programs(currProg).flatMap(prog => solveOneStep(prog, programs, visitedPrograms :+ currProg)).distinct
	} 

}
