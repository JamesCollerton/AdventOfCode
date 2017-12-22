import Utils._
import ThreeDVector._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashMap

object DayTwenty {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyTestInputTwo.txt")
		solveOne(input)
		solveTwo(input)
	}

	def solveOne(propertiesVectors: ArrayBuffer[PropertiesVector]): Unit = {
	
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

	def solveTwo(propertiesVectors: ArrayBuffer[PropertiesVector]): Unit = {

		val collisions = new LinkedHashMap[Int, ArrayBuffer[Int]]

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
				
					println(solnPos.x + ", " + solnPos.y + ", " + solnPos.z);
					println(solnNeg.x + ", " + solnNeg.y + ", " + solnNeg.z);
	
					if(solnPos.x == solnPos.y && solnPos.y == solnPos.z && !solnPos.x.isInfinite && solnPos.x >= 0) {
						if(!collisions.contains(solnPos.x.toInt)) collisions(solnPos.x.toInt) = new ArrayBuffer[Int]()
						collisions(solnPos.x.toInt) += i
						collisions(solnPos.x.toInt) += j
					} else if(solnNeg.x == solnNeg.y && solnNeg.y == solnNeg.z && !solnNeg.x.isInfinite && solnNeg.x >= 0) {
						if(!collisions.contains(solnNeg.x.toInt)) collisions(solnNeg.x.toInt) = new ArrayBuffer[Int]()
						collisions(solnNeg.x.toInt) += i
						collisions(solnNeg.x.toInt) += j
					}

				}
			}
		}

		val collidedParticles = new ArrayBuffer[Int]()
		val sortedCollisions = LinkedHashMap(collisions.toSeq.sortBy(_._1):_*)
		val numCollided = sortedCollisions.map{ case (time, particles) => 
			println("Key " + time + " Value " + particles.mkString(","))
			val collidingParticles = particles.distinct.diff(collidedParticles.distinct)
			if(collidingParticles.length >= 2) {
				collidedParticles ++ collidingParticles
				collidingParticles.length
			} else {
				0		
			}
		}.sum

		println("Solution " + (propertiesVectors.length - numCollided))

	} 

}
