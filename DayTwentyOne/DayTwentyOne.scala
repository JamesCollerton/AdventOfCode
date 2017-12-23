import Utils._
import ArtBook._

import scala.collection.mutable.ArrayBuffer

object DayTwentyOne {

	def main(args: Array[String]): Unit = {
		def image = Utils.readIn("OriginalImage.txt")
		def artBook = Utils.readIn("DayTwentyOneInput.txt")

		val artBookGrids = makeArtBookGrids(artBook)

		solveOne(image, artBookGrids)
	}

	def makeArtBookGrids(artBook: ArrayBuffer[Array[String]]): ArrayBuffer[ArtBook] = {
		artBook.map(bookString => new ArtBook(bookString.mkString("")))
	}

	def solveOne(image: ArrayBuffer[Array[String]], artBookGrids: ArrayBuffer[ArtBook]): Unit = {
		println("Sum " + solveOneStep(image, artBookGrids, 18))
	}

	def sumGrid(grid: ArrayBuffer[Array[String]]): Int = {
		grid.map(row => row.map(field => if(field == "#") 1 else 0).sum).sum
	}

	def solveOneStep(image: ArrayBuffer[Array[String]], artBookGrids: ArrayBuffer[ArtBook], counter: Int): Int = {
		if(counter == 0) return sumGrid(image)
		val imageSize = image.length;
		val (imageDivisor, imageModder) = if (imageSize % 2 == 0) {
			(imageSize / 2, 2)
		} else {
			(imageSize / 3, 3)
		}
		
		println()
		println("--------------------------------------------------------")
		println("Counter: " + counter)
	
		val grids = (0 to imageDivisor - 1).flatMap(i => {
			
			// Blocks of n rows
			val rows = image.slice(i * imageModder, (i + 1) * imageModder)
			
			// Block of n grids for the n rows
			val grids = (0 to imageDivisor - 1).map(j => {
				rows.map(_.slice(j * imageModder, (j + 1) * imageModder))		
			})

			grids
		})

		// Calculate new grids
		val newGrids = grids.map(grid => {
			val matchedGrids = artBookGrids.filter(artBookGrid => artBookGrid.compareGrid(grid))
			if(matchedGrids.length != 1) println("In trouble")
			matchedGrids(0).gridOutput
		})

		//Recombine grids
		val newGrid = (0 to imageDivisor - 1).flatMap(i => {
			
			// First n grids
			val nGrids = newGrids.slice(i * imageDivisor, (i + 1) * imageDivisor)
	
			// Combine rows of first n grids
			val nGridRows = (0 to nGrids(0).length - 1).map(j => {
				val newRow = nGrids.flatMap(grid => grid(j)).toArray
				newRow
			})

			nGridRows
		}).to[ArrayBuffer]

		println("--------------------------------------------------------")

		solveOneStep(newGrid, artBookGrids, counter - 1)

	}

}
