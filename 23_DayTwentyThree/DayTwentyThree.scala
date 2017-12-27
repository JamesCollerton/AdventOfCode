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
		//println()
		solveTwo(initialiseTwoRegisters())
	}

	def solveOne(instructions: ArrayBuffer[Instruction]): Unit = {
		
		val runnableZero = new ProgramThread(instructions) 
		val threadZero = new Thread(runnableZero)
		
		threadZero.start()

	}

	def initialiseTwoRegisters(): HashMap[String, Double] = {
		val registers = new HashMap[String, Double]

		('a' to 'h').foreach(ch => registers(ch.toString) = 0)

		//set b 84
		//set c b
		//jnz a 2
		//jnz 1 5
		//mul b 100
		//sub b -100000
		//set c b
		//sub c -17000

		registers("b") = (84 * 100) + 100000
		registers("c") = (84 * 100) + 100000 + 17000
		
		//registers("b") = 84
		//registers("c") = 84

		registers
	}

	var counter = 0

	def solveTwo(registers: HashMap[String, Double]): Unit = {

		//set f 1
		//set d 2

		registers("f") = 1
		registers("d") = 2
		registers("e") = 2

		println()
		println("Registers f, d and e initialised")
		println()

		blocks(registers)		

		println("H " + registers("h"))

	}

	def blocks(registers: HashMap[String, Double]): Unit = {
		blockOne(registers)
		val blockTwoFinished = blockTwo(registers)
		if(!blockTwoFinished) {
			registers("e") = 2
			blocks(registers)
		}
		if(!blockThree(registers)) solveTwo(registers)
	}

	@annotation.tailrec
	def blockOne(registers: HashMap[String, Double]): Unit = {

		//set g d
		//mul g e
		//sub g b
		//jnz g 2
		//set f 0
		//sub e -1
		//set g e
		//sub g b
		
		registers("g") = registers("d") * registers("e") - registers("b")
		if(registers("g") == 0) registers("f") = 0
		registers("e") += 1
		registers("g") = registers("e") - registers("b")
	
		println()
		println("Block one loop gone round")
		registers.foreach{ case(k, v) => print(" Key " + k + " Value " + v) }
		println()
		
		if(registers("g") == 0) return

		blockOne(registers)
	}

	def blockTwo(registers: HashMap[String, Double]): Boolean = {

		//sub d -1
		//set g d
		//sub g b
		//jnz g -13
		
		registers("d") += 1
		registers("g") = registers("d") - registers("b")

		println()
		println("Block two loop gone round")
		registers.foreach{ case(k, v) => print(" Key " + k + " Value " + v) }
		println()

		if(registers("g") == 0) return true
		else false

	}

	def blockThree(registers: HashMap[String, Double]): Boolean = {

		//jnz f 2
		//sub h -1
		//set g b
		//sub g c
		//jnz g 2
		//jnz 1 3
		//sub b -17
		//jnz 1 -23

		if(registers("f") == 0) registers("h") += 1
		registers("g") = registers("b") - registers("c")
		if(registers("g") == 0) {
			println(registers("h"))
			return true
		} else {
			registers("b") -= 17
			return false
		}

	}

}
