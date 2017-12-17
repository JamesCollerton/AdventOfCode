import scala.collection.mutable.ArrayBuffer

object DaySeventeen {

	def main(args: Array[String]): Unit = {
		solveOne()
	}
	
	def solveOne(): Unit = {
		val step = 356;
		println("Solve step " + solveOneStep(ArrayBuffer(0), 0, 1, step))
	}

	@annotation.tailrec
	def solveOneStep(buffer: ArrayBuffer[Int], currPos: Int, currValue: Int, step: Int): ArrayBuffer[Int] = {
		if(currValue == 1000000) return buffer
		//println()
		//println("Current value " + currValue)
		val modStep = step % buffer.length
		//println("Mod step " + modStep)
		//println("Curr pos " + currPos)
		val nextPosition = findNextPosition(buffer.length, currPos, modStep)
		//println("Next position " + nextPosition)
		//if(nextPosition > buffer.length - 1) buffer += 0
		buffer.insert(nextPosition, currValue)
		//println("New buffer " + buffer.mkString(","))
		solveOneStep(buffer, nextPosition, currValue + 1, step)
	}

	@annotation.tailrec
	def findNextPosition(bufferLength: Int, currPos: Int, modStep: Int) : Int = {
		if(currPos + modStep < bufferLength) return currPos + modStep + 1
		val toEnd = bufferLength - 1 - currPos
		//println("To End " + toEnd)
		val remainingStep = modStep - toEnd - 1
		//println("Remaining step " + remainingStep)
		findNextPosition(bufferLength, 0, remainingStep)
	}

}
