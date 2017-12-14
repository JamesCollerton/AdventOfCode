import Utils._
import DayTen._

import scala.collection.mutable.ArrayBuffer

object DayFourteen {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayFourteenTestInput.txt")(0)
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
	
		//gridArray.foreach(row => {
		//	row.foreach(field => print(field.toString))
		//	println()
		//})
	
		gridArray

	}

	def findRegions(hashGrid: Array[Array[Int]], regionGrid: Array[Array[Int]]): Int = {
		var regionCounter = 0;		

		for(i <- 1 to hashGrid.length - 2) {
			val row = hashGrid(i)
			for(j <- 1 to row.length - 2) {
				if(row(j) == 1) {
					regionCounter = searchRegionGrid(i, j, regionGrid, regionCounter)
		//			println()
		//			regionGrid.foreach(row => println(row.mkString(" ")))
					//println(regionCounter)
					//println()
				} 
				//println(regionCounter)
			}
		}
		
		regionGrid.foreach(row => println(row.mkString(" ")))

	//	regionGrid.foreach(row => println(row))
		println("Region counter " + regionCounter)
		0
	}

	def searchRegionGrid(i: Int, j: Int, regionGrid: Array[Array[Int]], regionCounter: Int): Int = {

		val above = regionGrid(i - 1)(j)
		val left = regionGrid(i)(j - 1)

		val newValue = if(above != 0) { 
			println("HEre")
			regionGrid(i)(j) = above 
		} else if(left != 0) {
			println("Here")
			regionGrid(i)(j) = left 
		} else { 
			regionGrid(i)(j) = regionCounter + 1
			return regionCounter + 1
		}

		return regionCounter
	
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
