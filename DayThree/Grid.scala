import scala.collection.mutable.ArrayBuffer

object Grid {

	// Prints all rows for a grid
	def printGrid[A](grid: ArrayBuffer[ArrayBuffer[A]]): Unit = {
		println()
		grid.foreach(row => println(row.mkString(" ")))
	}

	// Starts the grid drawing. We pad the grid with zeros twice to 
	// simplify the problem of adding surrounding squares
	def draw(): Unit = {
		move(addZeros(addZeros(ArrayBuffer(ArrayBuffer(1)))), (0, 0), 0, 325489);
	}

	// This pads the grid with zeros
	def addZeros(grid: ArrayBuffer[ArrayBuffer[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
		var tempGrid = ArrayBuffer(ArrayBuffer.fill(grid.size + 2)(0));
		grid.map(0 +: _ :+ 0).foreach(tempGrid.append(_))
		tempGrid.append(ArrayBuffer.fill(grid.size + 2)(0))	
		tempGrid
	}

	// Sums all of the surrounding squares
	def sumOutsides(grid: ArrayBuffer[ArrayBuffer[Int]], coord: (Int, Int)): Int = {
		val outside = for {
					i <- -1 to 1
					j <- -1 to 1 if !(i == j && i == 0 && j == 0)
				 } yield {
					grid(getYCoordAdj(grid, coord._2 + i))(getXCoordAdj(grid, coord._1 + j))
				 }
		outside.sum
	}

	// Takes in a coordinate, scans the outsides of that coordinate
	// for the result and then adds it into the grid. If we have
	// finished (the result is greater) then prints and exits.
	def calcNewGrid(grid: ArrayBuffer[ArrayBuffer[Int]], coords: (Int, Int), target: Int): ArrayBuffer[ArrayBuffer[Int]] = {

		val outsideSum = sumOutsides(grid, coords)

		if(outsideSum > target){
			println(outsideSum)
			System.exit(0)
		}

		grid(getYCoordAdj(grid, coords._2))(getXCoordAdj(grid, coords._1)) = outsideSum

		printGrid(grid)
		println("" + getYCoordAdj(grid, coords._1) + " " + (getXCoordAdj(grid, coords._2)))
		println(outsideSum)

		grid
	}

	// 330785

	// I did the maths with (0,0) at the centre, so this adjusts
	// for it in the array of arrays.
	def getXCoordAdj(grid: ArrayBuffer[ArrayBuffer[Int]], x: Int): Int = {
		grid.size / 2 + x
	}

	// Same as for the x adjustment, but also flips because we
	// count from the top
	def getYCoordAdj(grid: ArrayBuffer[ArrayBuffer[Int]], y: Int): Int = {
		grid.size/2 - y
	}

	// This makes the certain number of moves in the given direction
	// according to the input counter and coordinate increments
	def makeMove(	grid: ArrayBuffer[ArrayBuffer[Int]], 
			coords: (Int, Int), 
			coordIncr: (Int, Int),  
			counter: Int, 
			target: Int): ((Int, Int), ArrayBuffer[ArrayBuffer[Int]])  = {

		var tempCoords = coords
		var tempGrid = grid
		for(i <- 1 to counter){
			tempCoords = (tempCoords._1 + coordIncr._1, tempCoords._2 + coordIncr._2)
			tempGrid = calcNewGrid(tempGrid, tempCoords, target)
		}
		(tempCoords, tempGrid)

	}

	// This makes one lap of the spiral
	def move(grid: ArrayBuffer[ArrayBuffer[Int]], coords: (Int, Int), counter: Int, target: Int): Unit = {
		printGrid(grid)

		// Increment counter		
		var tempCounter = counter + 1;
		
		val(rightCoords, 	rightGrid) 	= makeMove(grid, coords, (1, 0), tempCounter, target)
		val(upCoords, 		upGrid) 	= makeMove(rightGrid, rightCoords, (0, 1), tempCounter, target)

		tempCounter += 1;
	
		val(leftCoords, 	leftGrid) 	= makeMove(upGrid, upCoords, (-1, 0), tempCounter, target)
		val(downCoords, 	downGrid) 	= makeMove(leftGrid, leftCoords, (0, -1), tempCounter, target)

		move(addZeros(downGrid), downCoords, tempCounter, target)

	} 

}
