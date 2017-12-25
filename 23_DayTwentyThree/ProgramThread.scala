package ProgramThread

import Instruction._

import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class ProgramThread(instructionsInput: ArrayBuffer[Instruction]) extends Runnable {

	var programCounter = 0
//	var isWaiting = false;
//	var otherThread: ProgramThread = null
	
//	val queue = new Queue[String];

	def run(): Unit = {
		println("Thread: " + solveOneStep(HashMap("a" -> 1), 0, instructionsInput))
	}

	@annotation.tailrec
	private def solveOneStep(registers: HashMap[String, Double], currPosition: Int, instructions: ArrayBuffer[Instruction]): Double = {
		
		// If we're off the edge
		if(currPosition < 0 || currPosition >= instructions.length) return programCounter

		// Get current instruction
		val currInstruction = instructions(currPosition)

		// Get amount and current value
		val amountValue = getAmountValue(registers, currInstruction.amount)
		val currentValue = getAmountValue(registers, currInstruction.register)
		
		// Setting current register
		if(registers.contains(currInstruction.register)) {
			registers(currInstruction.register) = currInstruction.instruction match {
				case "set" => amountValue
				//case "add" => currentValue + amountValue
				case "sub" => currentValue - amountValue
				case "mul" => {
					programCounter += 1
					currentValue * amountValue
				}
				//case "mod" => currentValue % amountValue
				//case "rcv" => getAmountValue(registers, receiveFunction())
				case _ => registers(currInstruction.register)
			}
		}

		// Do we end
		//if(isWaiting && otherThread.isWaiting) {
		//	println("" + startNum + ": " + "Current registers " + registers.map(x => "(" + x._1 +"," + x._2 +")").mkString(", ")) 
		//	return programCounter
		//}

		// Setting last sound
		//currInstruction.instruction match {
		//	case "snd" => queue.enqueue(currentValue.toString)
		//	case _ => ""
		//}

		// Setting next position
		val nextPosition = currInstruction.instruction match {
			case "jnz" => if(currentValue != 0) currPosition + amountValue.toInt else currPosition + 1
			case _ => currPosition + 1
		}

		solveOneStep(registers, nextPosition, instructions)
	}	

//	def receiveFunction(): String = {
//		if(otherThread.queue.length > 0) {
//			programCounter += 1
//			isWaiting = false
//			return otherThread.queue.dequeue()
//		}
//		Thread.sleep(100)
//		isWaiting = true
//		if(otherThread.isWaiting) {
//			return "!"
//		}
//		receiveFunction()
//	}

	def getAmountValue(registers: HashMap[String, Double], amount: String): Double = {
		val amountValue = try {
			amount.toDouble
		} catch {
			case _: Throwable => {
				val register = amount
				if(register != null) {
					if(!registers.contains(register)) registers(register) = 0
					registers(register)
				} else {
					0
				}
			}
		}
		amountValue
	}

//	def setOtherThread(programThread: ProgramThread): Unit = {
//		this.otherThread = programThread
//	}

}
