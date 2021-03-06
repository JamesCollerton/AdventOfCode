import Utils._

object DayEleven {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayElevenInput.txt")(0)
		solveOne(input)
	}

	// Using https://www.redblobgames.com/grids/hexagons/
	// and cube coordinates
	def solveOne(input: Array[String]): Unit = {
		val maxDistance = solveOneStep(input, (0, 0, 0), 0)
		println("Max distance is " + maxDistance)
	}

        @annotation.tailrec
	def solveOneStep(input: Array[String], coords: (Int, Int, Int), maxDistance: Int): Int = {
                if(input.length <= 0) return maxDistance
		val newCoords = input(0) match {
			case "nw" 	=> (coords._1 - 1, coords._2 + 1, coords._3)
			case "n" 	=> (coords._1, coords._2 + 1, coords._3 - 1)
			case "ne" 	=> (coords._1 + 1, coords._2, coords._3 - 1)
			case "se" 	=> (coords._1 + 1, coords._2 - 1, coords._3)
			case "s" 	=> (coords._1, coords._2 - 1, coords._3 + 1)
			case "sw" 	=> (coords._1 - 1, coords._2, coords._3 + 1)
		}
		val newDistance = (Math.abs(newCoords._1) + Math.abs(newCoords._2) + Math.abs(newCoords._3)) / 2
		solveOneStep(input.tail, newCoords, newDistance.max(maxDistance))  
	}

}
