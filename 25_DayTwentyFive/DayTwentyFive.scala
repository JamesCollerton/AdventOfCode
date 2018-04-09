import scala.collection.mutable.HashSet

object DayTwentyFive {

	def main(args: Array[String]): Unit = {
		solveOne()
	}

	def solveOne(): Unit = {
		val tickedBoxes = new HashSet[Int]
		println("Check sum " + solveOneStep(12368930, 0, "A", tickedBoxes))
	}

	def solveOneStep(counter: Int, position: Int, state: String, tickedBoxes: HashSet[Int]): Int = {
		if (counter == 0) return tickedBoxes.size
		val (newPosition, newState) = state match {
			case "A" => {
				if(tickedBoxes.contains(position)) {
					tickedBoxes -= position
					(position + 1, "C")	
				} else {
					tickedBoxes += position
					(position + 1, "B")
				}
			}
			case "B" => {
				if(tickedBoxes.contains(position)) {
					tickedBoxes -= position
					(position + 1, "D")
				} else {
					(position - 1, "A")
				}
			}
			case "C" => {
				if(tickedBoxes.contains(position)) {
					(position + 1, "A")
				} else {
					tickedBoxes += position
					(position + 1, "D")
				}
			}
			case "D" => {
				if(tickedBoxes.contains(position)) {
					tickedBoxes -= position
					(position - 1, "D")
				} else {
					tickedBoxes += position
					(position - 1, "E")
				}
			}
			case "E" => {
				if(tickedBoxes.contains(position)) {
					(position - 1, "B")
				} else {
					tickedBoxes += position
					(position + 1, "F")
				}
			}
			case "F" => {
				if(tickedBoxes.contains(position)) {
					(position + 1, "E")
				} else {
					tickedBoxes += position
					(position + 1, "A")
				}
			}
			case _ => {
				println("In trouble")
				(position, state)
			}
		}
		solveOneStep(counter - 1, newPosition, newState, tickedBoxes)
	}

}
