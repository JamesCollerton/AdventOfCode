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

	def calculateCoefficient(vecOne: PropertiesVector, vecTwo: PropertiesVector)(f: (Double, Double, Double, Double, Double, Double) => Double): ThreeDVector[Double] = {
		val x = f(vecOne.a.x, vecTwo.a.x, vecOne.v.x, vecTwo.v.x, vecOne.p.x, vecTwo.p.x)
		val y = f(vecOne.a.y, vecTwo.a.y, vecOne.v.y, vecTwo.v.y, vecOne.p.y, vecTwo.p.y)
		val z = f(vecOne.a.z, vecTwo.a.z, vecOne.v.z, vecTwo.v.z, vecOne.p.z, vecTwo.p.z)
		new ThreeDVector(x, y, z)
	}

	// All functions will be of the form a1, a2, v1, v2, p1, p2

	def solveTwo(propertiesVectors: ArrayBuffer[PropertiesVector]): Unit = {

		// Calculate every possible collision between two particles
		for(i <- 0 to propertiesVectors.length - 1) {
			for(j <- i to propertiesVectors.length - 1) {
				if(i != j) {
					val vecOne = propertiesVectors(i)
					val vecTwo = propertiesVectors(j)

					def aFunc(a1: Double, a2: Double, v1: Double, v2: Double, p1: Double, p2: Double): Double = {
						0.5 * a1 - 0.5 * a2
					}
					val a = calculateCoefficient(vecOne, vecTwo)(aFunc)

					def bFunc(a1: Double, a2: Double, v1: Double, v2: Double, p1: Double, p2: Double): Double = {
						0.5 * (a1 - a2) + (v1 - v2)
					}
					val b = calculateCoefficient(vecOne, vecTwo)(bFunc)

					def cFunc(a1: Double, a2: Double, v1: Double, v2: Double, p1: Double, p2: Double): Double = {
						p1 - p2
					}
					val c = calculateCoefficient(vecOne, vecTwo)(cFunc)
					//val ax = 0.5 * vecOne.a.x - 0.5 * vecTwo.a.x
					//val ay = 0.5 * vecOne.a.y - 0.5 * vecTwo.a.y
					//val az = 0.5 * vecOne.a.z - 0.5 * vecTwo.a.z
					//val a = new ThreeDVector(ax, ay, az)

					//val bx = 0.5 * (vecOne.a.x - vecTwo.a.x) + (vecOne.v.x - vecTwo.v.x)
					//val by = 0.5 * (vecOne.a.y - vecTwo.a.y) + (vecOne.v.y - vecTwo.v.y)
					//val bz = 0.5 * (vecOne.a.z - vecTwo.a.z) + (vecOne.v.z - vecTwo.v.z)
					//val b = new ThreeDVector(bx, by, bz)

					//val cx = vecOne.p.x - vecTwo.p.x
					//val cy = vecOne.p.y - vecTwo.p.y
					//val cz = vecOne.p.z - vecTwo.p.z
					//val c = new ThreeDVector(cx, cy, cz)

					// [-b +/- sqrt(b^2 -4ac)] / 2a	
					val solnPosx = (-b.x + Math.sqrt(Math.pow(b.x, 2) - 4 * a.x * c.x)) / 2 * a.x
					val solnPosy = (-b.y + Math.sqrt(Math.pow(b.y, 2) - 4 * a.y * c.y)) / 2 * a.y
					val solnPosz = (-b.z + Math.sqrt(Math.pow(b.z, 2) - 4 * a.z * c.z)) / 2 * a.z

					//println()
					//if(2.0 == 2.0) println("OK")
					if(solnPosx == solnPosy && solnPosy == solnPosz) {
						println("Here")
						println("Time solutions " + solnPosx + ", " + solnPosy + ", " + solnPosz)
					}
					
					val solnNegx = (-b.x - Math.sqrt(Math.pow(b.x, 2) - 4 * a.x * c.x)) / 2 * a.x
					val solnNegy = (-b.y - Math.sqrt(Math.pow(b.y, 2) - 4 * a.y * c.y)) / 2 * a.y
					val solnNegz = (-b.z - Math.sqrt(Math.pow(b.z, 2) - 4 * a.z * c.z)) / 2 * a.z

					if(solnNegx == solnNegy && solnNegy == solnNegz) {
						println("Here")
						println("Time solutions " + solnPosx + ", " + solnPosy + ", " + solnPosz)
						println("" + i + ", " + j)
					}
				}
			}
		}

	} 

}
