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
		println() 
		gridRows.foreach(row => println(row.mkString("")))
		println()
		val gridCols = (0 to grid.length - 1).map(i => grid.map(row => row(i)))
		gridCols.foreach(col => println(col.mkString("")))
		//val outputRows = grid
		//val outputCols = grid.map(
		true
	}

}
