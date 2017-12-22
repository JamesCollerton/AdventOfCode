package ArtBook

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

}
