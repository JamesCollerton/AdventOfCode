import scala.collection.mutable.ArrayBuffer

object DaySeventeen {

	def main(args: Array[String]): Unit = {
		solveOne()
	}

	def solveOne(): Unit = {
		val step = 356;
		println("Solve step " + solveOneStep(0, 0, 1, step))
	}
	
	@annotation.tailrec
	def solveOneStep(currFirstValue: Int, currPos: Int, currValue: Int, step: Int): Int = {
		if(currValue == 50000000) return currFirstValue
		val modStep = step % currValue
		val nextPosition = findNextPosition(currValue, currPos, modStep)
		val newFirstValue = if(nextPosition == 1) currValue else currFirstValue
		solveOneStep(newFirstValue, nextPosition, currValue + 1, step)
	}

	@annotation.tailrec
	def findNextPosition(bufferLength: Int, currPos: Int, modStep: Int) : Int = {
		if(currPos + modStep < bufferLength) return currPos + modStep + 1
		val toEnd = bufferLength - 1 - currPos
		val remainingStep = modStep - toEnd - 1
		findNextPosition(bufferLength, 0, remainingStep)
	}

}
