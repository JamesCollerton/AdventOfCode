package ArtBook

import scala.collection.mutable.ArrayBuffer

class ArtBook(val artBookString: String) {

	// The grid we are matching
	val gridInput = createGridInput(artBookString)
	val gridOutput = createGridOutput(artBookString)
	val allMatchingGrids = createAllMatchingGrids(gridInput)

	def createGridInput(str: String): Array[Array[String]] = {
		createGrid(str.split("=>")(0).trim)
	}

	def createGridOutput(str: String): Array[Array[String]] = {
		createGrid(str.split("=>")(1).trim)
	}

	def createGrid(str: String): Array[Array[String]] = {
		str.split("/").map(_.split(""))
	}

	def createAllMatchingGrids(inputGrid: Array[Array[String]]): ArrayBuffer[Array[Array[String]]] = {
		val matchingGrids = ArrayBuffer(inputGrid)
	
		matchingGrids += symmetric(matchingGrids.last.clone)
		matchingGrids += flip(matchingGrids.last.clone)
		matchingGrids += symmetric(matchingGrids.last.clone)
		matchingGrids += flip(matchingGrids.last.clone)
		matchingGrids += symmetric(matchingGrids.last.clone)
		matchingGrids += flip(matchingGrids.last.clone)
		matchingGrids += symmetric(matchingGrids.last.clone)
		matchingGrids += flip(matchingGrids.last.clone)
	
		matchingGrids.distinct
	}

	def symmetric(grid: Array[Array[String]]): Array[Array[String]] = {
		val newGrid = Array.ofDim[String](grid.length, grid.length) 
		for(i <- 0 to newGrid.length - 1) {
			for(j <- 0 to newGrid.length - 1){
				newGrid(i)(j) = grid(j)(i)
			}
		}
		newGrid
	}

	def flip(grid: Array[Array[String]]): Array[Array[String]] = {
		val reverseY = grid.reverse
		reverseY
	}

	def printInput(): Unit = {
		println()
		println("Input")
		gridInput.foreach(row => println(row.mkString("")))
	}
	
	def printOutput(): Unit = {
		println()
		println("Output")
		gridOutput.foreach(row => println(row.mkString("")))
	}

	def sumGrid(grid: Array[Array[String]]): Int = {
		grid.map(row => row.map(field => if(field == "#") 1 else 0).sum).sum
	}

	def compareGrid(grid: ArrayBuffer[Array[String]]): Boolean = {

		allMatchingGrids.foreach(matchingGrid => {
			if(grid.toArray.deep == matchingGrid.toArray.deep) {
				return true
			}
		})

		false
	}

}
