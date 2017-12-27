import Utils._
import Instruction._
import ProgramThread._

import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

object DayTwentyThree {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyThreeInput.txt")
	//	solveOne(input)	
		equivalentTest()
	}

	def solveOne(instructions: ArrayBuffer[Instruction]): Unit = {
		
		val runnableZero = new ProgramThread(instructions) 
		val threadZero = new Thread(runnableZero)
		
		threadZero.start()

	}

	def equivalentTest(): Unit = {

		val numOne = (84 * 100) + 100000
		val numTwo = (84 * 100) + 100000 + 17000
		var counter = 0  

		val allNumbers = (numOne to numTwo by 17)

		val compositeNums = allNumbers.map(num => {
			if (checkPrime(num)) 0
			else 1
		}).sum

		println()
		println("Sum " + compositeNums)

	}

	def checkPrime(num: Int): Boolean = {
		for(i <- 2 to num - 1) {
			if(num % i == 0) {
				return false
			}
		}
		return true
	}

}
