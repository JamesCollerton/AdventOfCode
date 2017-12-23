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
		//val gridCols = (0 to grid.length - 1).map(i => grid.map(row => row(i)).toArray)
		val inputRows = gridInput
		val inputCols = (0 to gridInput.length - 1).map(i => gridInput.map(row => row(i)).toArray)

		// Either the columns of the input grid are the same as the rows or the rows are.

		val gridRowsAndCols = gridRows ++ gridCols
		
		val inputRowsAndColsAndReversed = inputRows ++ inputCols ++ inputRows.reverse ++ inputCols.reverse

		val numIntersects = gridRowsAndCols.map(gridRow => {
			inputRowsAndColsAndReversed.map(inputRow => {
				if(inputRow.deep == gridRow.deep) {
					//println("Intersect one " + gridRow.mkString(""))
					//println("Intersect two " + inputRow.mkString(""))
					1
				} else {
					0
				}
			}).sum 
		}).sum
		//val intersectingRowsAndCols = gridRowsAndCols.intersect(inputRowsAndColsAndReversed)

		println()
		println("Num intersects " + numIntersects)

		true
	}

}
