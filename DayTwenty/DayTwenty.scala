import Utils._
import ThreeDVector._

import scala.collection.mutable.ArrayBuffer

object DayTwenty {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyInput.txt")
		solveOne(input)
		solveTwo(input)
	}

	def solveOne(propertiesVectors: ArrayBuffer[PropertiesVector]): Unit = {
	
		// Index of particle with lowest acceleration. Fluke because there's only one
		// particle with min acceleration.	
		println("Min acceleration " + propertiesVectors.map(_.getAcceleration()).zipWithIndex.min._2)

	}

	def solveTwo(propertiesVectors: ArrayBuffer[PropertiesVector]): Unit = {

		// Calculate every possible collision between two particles
		for(i <- 0 to propertiesVectors.length - 1) {
			for(j <- 0 to propertiesVectors.length - 1) {
				if(i != j) {
					val vecOne = propertiesVectors(i)
					val vecTwo = propertiesVectors(j)

					val ax = 0.5 * vecOne.a.x - 0.5 * vecTwo.a.x
					val ay = 0.5 * vecOne.a.y - 0.5 * vecTwo.a.y
					val az = 0.5 * vecOne.a.z - 0.5 * vecTwo.a.z
					val a = new ThreeDVector(ax, ay, az)

					val bx = 0.5 * (vecOne.a.x - vecTwo.a.x) + (vecOne.v.x - vecTwo.v.x)
					val by = 0.5 * (vecOne.a.y - vecTwo.a.y) + (vecOne.v.y - vecTwo.v.y)
					val bz = 0.5 * (vecOne.a.z - vecTwo.a.z) + (vecOne.v.z - vecTwo.v.z)
					val b = new ThreeDVector(bx, by, bz)

					val cx = vecOne.p.x - vecTwo.p.x
					val cy = vecOne.p.y - vecTwo.p.y
					val cz = vecOne.p.z - vecTwo.p.z
					val c = new ThreeDVector(cx, cy, cz)

					// [-b +/- sqrt(b^2 -4ac)] / 2a	
					//val solnPosx = -b.x + Math.sqrt(b.x^2 - 4 * a.x * c.x)
					//val solnPosy = 
					//val solnPosz = 

					//val solnNeg =
				}
			}
		}

	} 

}
