package Instruction

class Instruction(instructionString: String) {

	private val splitString = instructionString.split(" ")

	val instruction = splitString(0).trim
	val register = splitString(1).trim
	val amount = if(splitString.length > 2){ splitString(2).trim } else null

	def printline(): Unit = {
		println(instruction + " " + register + " " + amount)
	}

}
