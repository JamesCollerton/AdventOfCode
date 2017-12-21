import Utils._
import ThreeDVector._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListMap

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
	
	def calculateSolution(a: ThreeDVector[Double], b: ThreeDVector[Double], c: ThreeDVector[Double])(f: (Double, Double, Double) => Double): ThreeDVector[Double] = {
		val x = f(a.x, b.x, c.x)
		val y = f(a.y, b.y, c.y)
		val z = f(a.z, b.z, c.z)
		new ThreeDVector(x, y, z)
	}

	// All functions will be of the form a1, a2, v1, v2, p1, p2

	def solveTwo(propertiesVectors: ArrayBuffer[PropertiesVector]): Unit = {

		val collisions = new ListMap[Int, (Int, Int)]

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

					def posSolnFunc(a: Double, b: Double, c: Double): Double = {
						(-b + Math.sqrt(Math.pow(b, 2) - 4 * a * c)) / (2 * a)
					}
					val solnPos = calculateSolution(a, b, c)(posSolnFunc)

					def negSolnFunc(a: Double, b: Double, c: Double): Double = {
						(-b - Math.sqrt(Math.pow(b, 2) - 4 * a * c)) / (2 * a)
					}
					val solnNeg = calculateSolution(a, b, c)(negSolnFunc)
					
					if(solnPos.x == solnPos.y && solnPos.y == solnPos.z && !solnPos.x.isInfinite && solnPos.x >= 0) {
					//	println("Solution " + solnPos.x + ", " + solnPos.y + ", " + solnPos.z)
						collisions(solnPos.x.toInt) = (i, j)	
					} else if(solnNeg.x == solnNeg.y && solnNeg.y == solnNeg.z && !solnNeg.x.isInfinite && solnNeg.x >= 0) {
					//	println("Solution " + solnNeg.x + ", " + solnNeg.y + ", " + solnNeg.z)
						collisions(solnNeg.x.toInt) = (i, j)
					}

					
	
					// [-b +/- sqrt(b^2 -4ac)] / 2a	
					//val solnPosx = (-b.x + Math.sqrt(Math.pow(b.x, 2) - 4 * a.x * c.x)) / 2 * a.x
					//val solnPosy = (-b.y + Math.sqrt(Math.pow(b.y, 2) - 4 * a.y * c.y)) / 2 * a.y
					//val solnPosz = (-b.z + Math.sqrt(Math.pow(b.z, 2) - 4 * a.z * c.z)) / 2 * a.z

					//println()
					//if(2.0 == 2.0) println("OK")
					//if(solnPosx == solnPosy && solnPosy == solnPosz) {
					//	println("Here")
					//	println("" + solnPos.x + ", " + solnPos.y + ", " + solnPos.z)
					//	println("Time solutions " + solnPosx + ", " + solnPosy + ", " + solnPosz)
					//}
					
					//val solnNegx = (-b.x - Math.sqrt(Math.pow(b.x, 2) - 4 * a.x * c.x)) / 2 * a.x
					//val solnNegy = (-b.y - Math.sqrt(Math.pow(b.y, 2) - 4 * a.y * c.y)) / 2 * a.y
					//val solnNegz = (-b.z - Math.sqrt(Math.pow(b.z, 2) - 4 * a.z * c.z)) / 2 * a.z

					//if(solnNegx == solnNegy && solnNegy == solnNegz) {
					//	println("Here")
					//	println("Time solutions " + solnPosx + ", " + solnPosy + ", " + solnPosz)
					//	println("" + i + ", " + j)
					//}
				}
			}
		}

		val collidedParticles = new ArrayBuffer[Int]()
		val sortedCollisions = ListMap(collisions.toSeq.sortWith(_._1 < _._1):_*)
		sortedCollisions.foreach(collision => println(collision))

	} 

}
