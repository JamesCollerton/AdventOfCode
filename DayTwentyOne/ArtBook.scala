package ArtBook

import scala.collection.mutable.ArrayBuffer

class ArtBook(val artBookString: String) {

	// The grid we are matching
	val gridInput = createGridInput(artBookString)
	val gridOutput = createGridOutput(artBookString)

	def createGridInput(str: String): Array[Array[String]] = {
		createGrid(str.split("=>")(0).trim)
	}

	def createGridOutput(str: String): Array[Array[String]] = {
		createGrid(str.split("=>")(1).trim)
	}

	def createGrid(str: String): Array[Array[String]] = {
		str.split("/").map(_.split(""))
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

	def compareGrid(grid: ArrayBuffer[Array[String]]): Boolean = {
		val gridRows = grid
		val inputRows = gridInput
		val inputCols = (0 to gridInput.length - 1).map(i => gridInput.map(row => row(i)).toArray)

		// Either the columns of the input grid are the same as the rows or the rows are.
		val matchingRows = gridRows.map(gridRow => {
			inputRows.map(inputRow => {
				if(inputRow.deep == gridRow.deep || inputRow.deep == gridRow.reverse.deep) 1 else 0
			}).sum
		}).sum

		//println("Matched rows: " + matchingRows)
		//println()

		if(matchingRows == gridRows.length) {
			println("Matched rows")
			return true
		}

		val matchingCols = gridRows.map(gridRow => {
			inputCols.map(inputCol => {
				if(inputCol.deep == gridRow.deep || inputCol.deep == gridRow.reverse.deep) 1 else 0
			}).sum
		}).sum

		//println("Matched cols: " + matchingCols)
		//println()

		if(matchingCols == gridRows.length) {
			println("Matched cols")
			return true
		}

		false
	}

}
