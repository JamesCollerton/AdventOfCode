import Utils._
import DayTen._

import scala.collection.mutable.ArrayBuffer

object DayFourteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayFourteenInput.txt")(0)
		val hashGrid = convertToArrayGrid(getHashGrid(input))
		val regionGrid = convertToArrayGrid(getRegionGrid())
		findRegions(hashGrid, regionGrid)
	}

	def convertToArrayGrid(listGrid: Array[String]): Array[Array[Int]] = {
		
		val gridArray: Array[Array[Int]] = Array.ofDim[Int](130, 130)
		
		for(i <- 0 to listGrid.length - 1) {
			val row = listGrid(i)
			for(j <- 0 to row.length - 1) {
				gridArray(i)(j) = row.charAt(j).toString.toInt
			}
		}
	
		gridArray

	}

	def findRegions(hashGrid: Array[Array[Int]], regionGrid: Array[Array[Int]]): Unit = {
		var regionCounter = 1;		

		for(i <- 1 to hashGrid.length - 2) {
			val row = hashGrid(i)
			for(j <- 1 to row.length - 2) {
				if(row(j) == 1 && regionGrid(i)(j) == 0) {
					regionCounter = searchRegionGrid(i, j, regionCounter, hashGrid, regionGrid)
				} 
			}
		}
		
		regionGrid.foreach(row => println(row.mkString(" ")))
		println("Region counter " + (regionCounter - 1))
	}

	def searchRegionGrid(i: Int, j: Int, regionCounter: Int, hashGrid: Array[Array[Int]], regionGrid: Array[Array[Int]]): Int = {

		// Change current place on regionGrid
		if(hashGrid(i)(j) != 0 && regionGrid(i)(j) == 0) regionGrid(i)(j) = regionCounter

		// Above
		if(hashGrid(i - 1)(j) != 0 && regionGrid(i - 1)(j) == 0) searchRegionGrid(i - 1, j, regionCounter, hashGrid, regionGrid)

		// Right
		if(hashGrid(i)(j + 1) != 0 && regionGrid(i)(j + 1) == 0) searchRegionGrid(i, j + 1, regionCounter, hashGrid, regionGrid)

		// Below
		if(hashGrid(i + 1)(j) != 0 && regionGrid(i + 1)(j) == 0) searchRegionGrid(i + 1, j, regionCounter, hashGrid, regionGrid)

		// Left
		if(hashGrid(i)(j - 1) != 0 && regionGrid(i)(j - 1) == 0) searchRegionGrid(i, j - 1, regionCounter, hashGrid, regionGrid)

		regionCounter + 1	
	}

	def getHashGrid(input: String): Array[String] = {
		val hashGrid = (0 to 127).map(i => {
			val knotHash = calculateKnotHash(input + "-" + i)
			"0" + knotHash.map(ch => pad(Integer.parseInt(ch.toString, 16).toBinaryString)).mkString("") + "0"
		})
		(("0" * 130) +: hashGrid :+ ("0" * 130)).toArray
	}

	def getRegionGrid(): Array[String] = {
		List.fill(130)("0" * 130).toArray
	}

	def pad(input: String): String = {
		("0000" + input).takeRight(4)
	}

	def calculateKnotHash(toBeHashed: String): String = {
		val asciiArray = toBeHashed.map(ch => ch.toInt).toArray
		DayTen.solveTwo(asciiArray, 255)
	}

}
