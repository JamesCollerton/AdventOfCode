import Utils._

import scala.collection.mutable.HashMap

object DayTwelve {

	def main(args: Array[String]) = {
		val input = Utils.readIn("DayTwelveInput.txt")
		// solveOne(input)	
		solveTwo(input)
	}

	def solveOne(programs: HashMap[Int, Array[Int]]): Unit = {
		println("Number in group " + solveOneStep(0, programs, new Array(0)).length)
	}

	def solveTwo(programs: HashMap[Int, Array[Int]]): Unit = {
		val remainingPrograms = programs.keySet.toArray
		val allPrograms = programs.keySet.toArray
		println("Number of groups " + (solveTwoStep(programs, remainingPrograms) - 1))
	}

	def solveTwoStep(programs: HashMap[Int, Array[Int]], remainingPrograms: Array[Int]): Int = {
		if(remainingPrograms.length <= 0) return 0
		val newlyVisited = solveOneStep(remainingPrograms(0), programs, new Array(remainingPrograms(0)))
		solveTwoStep(programs, remainingPrograms diff newlyVisited) + 1
	}

	def solveOneStep(currProg: Int, programs: HashMap[Int, Array[Int]], visitedPrograms: Array[Int]): Array[Int] = {
		if (visitedPrograms.contains(currProg)) return visitedPrograms
		programs(currProg).flatMap(prog => solveOneStep(prog, programs, visitedPrograms :+ currProg)).distinct
	} 

}
