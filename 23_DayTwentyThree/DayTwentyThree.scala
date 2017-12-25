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
		
		val runnableZero = new ProgramThread(0.0, instructions) 
		val threadZero = new Thread(runnableZero)
		
		val runnableOne = new ProgramThread(1.0, instructions) 
		val threadOne = new Thread(runnableOne)

		runnableZero.setOtherThread(runnableOne)
		runnableOne.setOtherThread(runnableZero)
		
		threadZero.start()
		threadOne.start()

	}

}
