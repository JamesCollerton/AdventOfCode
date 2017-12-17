import scala.collection.mutable.ArrayBuffer

object DaySeventeen {

	def main(args: Array[String]): Unit = {
		solveOne()
	}
	
	def solveOne(): Unit = {
		val step = 3;
		solveOneStep(ArrayBuffer(0), 0, 1, step)
	}

	@annotation.tailrec
	def solveOneStep(buffer: ArrayBuffer[Int], currPos: Int, currValue: Int, step: Int): ArrayBuffer[Int] = {
		if(currValue == 5) return buffer
		println()
		val modStep = step % buffer.length
		println("Mod step " + modStep)
		println("Curr pos " + currPos)
		val nextPosition = if(currPos + modStep > buffer.length) {
			val toEnd = buffer.length - currPos
			println("To End " + toEnd)
			val remainingStep = modStep - toEnd
			println("Remaining step " + remainingStep)
			remainingStep + 1
		} else {
			currPos + modStep + 1
		}	
		println("Next position " + nextPosition)
		//if(nextPosition > buffer.length - 1) buffer += 0
		buffer.insert(nextPosition, currValue)
		println("New buffer " + buffer.mkString(","))
		solveOneStep(buffer, nextPosition, currValue + 1, step)
	}

	@annotation.tailrec
	def findNextStep(bufferLength: Int, currPos: Int, modStep: Int) : Int = {
		if(currPos + modStep < buffer.length) return currPos + modStep + 1
		val toEnd = buffer.length - currPos
		println("To End " + toEnd)
		val remainingStep = modStep - toEnd
		println("Remaining step " + remainingStep)
		findNextStep(bufferLength, currPos, remainingStep)
	}

}
