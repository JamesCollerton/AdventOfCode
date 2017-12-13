import Utils._
import scala.collection.mutable.HashMap

object DayThirteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayThirteenTestInputOne.txt")
		solveOne(input)
	}

	def solveOne(fireWall: HashMap[Int, (Int, Int)]): Unit = {
		val stopPosition = fireWall.map{ case(k, v) => k }.toArray.max
		println("Severity " + solveOneStep(0, fireWall, stopPosition))
	}

	def solveOneStep(currPos: Int, fireWall: HashMap[Int, (Int, Int)], stopPosition: Int): Int = {
		if(currPos > stopPosition) return 0
		val movedFireWall = fireWall.map{case(k, v)  => {
			// Need to implement flag for going back and forwards
			if(v._2 == v._1)
			if(v._2 == 0)
			(k, (v._1,  v._2 + 1))
		}
		if(movedFirewall.contains(currPos) && movedFirewall(currPos)._2 == 1) {
			solveOneStep(currPos + 1, movedFireWall, stopPosition) + currPos * movedFirewall(currPos)._1
		}
		solveOneStep(currPos + 1, movedFireWall, stopPosition) 
	}

}
