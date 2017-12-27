package ProgramThread

import Instruction._

import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class ProgramThread(instructionsInput: ArrayBuffer[Instruction]) extends Runnable {

	var programCounter = 0

	def run(): Unit = {
		println("Thread: " + solveOneStep(HashMap("a" -> 0), 0, instructionsInput))
	}

	@annotation.tailrec
	private def solveOneStep(registers: HashMap[String, Double], currPosition: Int, instructions: ArrayBuffer[Instruction]): Double = {
		
		// If we're off the edge
		if(currPosition < 0 || currPosition >= instructions.length) {
			println("H " + registers("h"))
			return programCounter
		}

		// Get current instruction
		val currInstruction = instructions(currPosition)

		// Get amount and current value
		val amountValue = getAmountValue(registers, currInstruction.amount)
		val currentValue = getAmountValue(registers, currInstruction.register)
		
		// Setting current register
		if(registers.contains(currInstruction.register)) {
			registers(currInstruction.register) = currInstruction.instruction match {
				case "set" => amountValue
				case "sub" => currentValue - amountValue
				case "mul" => {
					programCounter += 1
					currentValue * amountValue
				}
				case _ => registers(currInstruction.register)
			}
		}

		// Setting next position
		val nextPosition = currInstruction.instruction match {
			case "jnz" => if(currentValue != 0) currPosition + amountValue.toInt else currPosition + 1
			case _ => currPosition + 1
		}
		
		println()	
		currInstruction.printline()
		registers.foreach{ case(k, v) => print(" Key " + k + " Value " + v) }
		println()

		solveOneStep(registers, nextPosition, instructions)
	}	

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

}
