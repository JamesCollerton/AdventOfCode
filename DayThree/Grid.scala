import scala.collection.mutable.ArrayBuffer

object Grid {

	// Make sense to think of this with negative coordinates

	// R1, U1, L2, D2
	// R3, U3, L4, D4

	def printGrid[A](grid: ArrayBuffer[ArrayBuffer[A]]): Unit = {
		println()
		grid.foreach(row => println(row.mkString(" ")))
	}

	def draw(): Unit = {
		move(addZeros(addZeros(ArrayBuffer(ArrayBuffer(1)))), (0, 0), 0, 122);
	}

	def addZeros(grid: ArrayBuffer[ArrayBuffer[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
		var tempGrid = ArrayBuffer(ArrayBuffer.fill(grid.size + 2)(0));
		grid.map(0 +: _ :+ 0).foreach(tempGrid.append(_))
		tempGrid.append(ArrayBuffer.fill(grid.size + 2)(0))	
		tempGrid
	}

	def sumOutsides(grid: ArrayBuffer[ArrayBuffer[Int]], coord: (Int, Int)): Int = {
		val outside = for {
					i <- -1 to 1
					j <- -1 to 1 if (i != j && i != 0)
				 } yield {
					grid(getYCoordAdj(grid, coord._2 + i))(getXCoordAdj(grid, coord._1 + j))
				 }
		outside.sum  
	}

	// Note, you can probably simplify this problem by adding a cushioning
	// layer of zeros on the outside

	def calcNewGrid(grid: ArrayBuffer[ArrayBuffer[Int]], coords: (Int, Int), target: Int): ArrayBuffer[ArrayBuffer[Int]] = {

		val outsideSum = sumOutsides(grid, coords)
		if(outsideSum > target){
			println(outsideSum)
			System.exit(0)
		}

		grid(getYCoordAdj(grid, coords._2))(getXCoordAdj(grid, coords._1)) = 8 

		printGrid(grid)
		println("" + getYCoordAdj(grid, coords._1) + " " + (getXCoordAdj(grid, coords._2)))
		println(outsideSum)

		grid
	}

	def getXCoordAdj(grid: ArrayBuffer[ArrayBuffer[Int]], x: Int): Int = {
		if(grid.size % 2 == 0){
			grid.size / 2 + x
		} else {
			(grid.size - 1) / 2 + x
		}
	}

	def getYCoordAdj(grid: ArrayBuffer[ArrayBuffer[Int]], y: Int): Int = {
		if(grid.size % 2 == 0){
			grid.size - (grid.size/2 + y)
		} else {
			grid.size/2 - y
		}
	}

	def move(grid: ArrayBuffer[ArrayBuffer[Int]], coord: (Int, Int), counter: Int, target: Int): Unit = {
		printGrid(grid)

		// Increment counter		
		var tempCounter = counter + 1;
		var tempCoords = coord;
		var tempGrid = grid;

		// Move right counter times
		for(i <- 1 to tempCounter){
			tempCoords = (tempCoords._1 + 1, tempCoords._2)
			tempGrid = calcNewGrid(tempGrid, tempCoords, target)
		}
			
		// Move up counter times
		for(i <- 1 to tempCounter){
			tempCoords = (tempCoords._1, tempCoords._2 + 1)
			tempGrid = calcNewGrid(tempGrid, tempCoords, target)
		}

		// Increment counter
		tempCounter += 1

		// Move left counter times

		// Move down counter times

	} 

}
