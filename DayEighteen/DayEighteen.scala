import Utils._
import Instruction._
import ProgramThread._

import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

object DayEighteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayEighteenInput.txt")
		solveOne(input)	
	}

	def solveOne(instructions: ArrayBuffer[Instruction]): Unit = {
		
		val programZeroQueue = new Queue[Int]
		val programOneQueue = new Queue[Int]

		val runnableZero = new ProgramThread(0, instructions, programOneQueue, programZeroQueue) 
		val threadZero = new Thread(runnableZero)
		
		val runnableOne = new ProgramThread(0, instructions, programZeroQueue, programOneQueue) 
		val threadOne = new Thread(runnableOne)
		
		threadZero.start()
		threadOne.start()

	}

}
