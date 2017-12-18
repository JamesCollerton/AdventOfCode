import Utils._
import Instruction._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object DayEighteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayEighteenTestInput.txt")
		solveOne(input)	
	}

	def solveOne(instructions: ArrayBuffer[Instruction]): Unit = {
		
	}

	@annotation.tailrec
	def solveOneStep(registers: HashMap[String, Int], lastSound: Int, instructions: ArrayBuffer[Instruction]): Int = {
		if(instructions.length == 0) return lastSound
		val currInstruction = instructions(0)
		registers(currInstruction) = currInstruction(0) match {
			// To do
		}
		solveOneStep(registers, lastSound, instructions.tail)
	}	

}
