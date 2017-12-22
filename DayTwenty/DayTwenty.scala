import Utils._
import ThreeDVector._
import PropertiesVector._
import SolveEquations._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashMap

object DayTwenty {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayTwentyInput.txt")
		solveOne(input)
		solveTwo(input)
	}

	def solveOne(propertiesVectors: ArrayBuffer[PropertiesVector]): Unit = {
	
		println("Min acceleration " + propertiesVectors.map(_.getAcceleration()).zipWithIndex.min._2)

	}

	def solveTwo(propertiesVectors: ArrayBuffer[PropertiesVector]): Unit = {

		val collisions = new LinkedHashMap[Int, ArrayBuffer[Int]]

		// Calculate every possible collision between two particles
		for(i <- 0 to propertiesVectors.length - 1) {
			for(j <- i to propertiesVectors.length - 1) {
				if(i != j) {
					val vecOne = propertiesVectors(i)
					val vecTwo = propertiesVectors(j)
					
					SolveEquations.solveQuadratic(vecOne, vecTwo, collisions, i, j)

				}
			}
		}

		findNumberCollisions(collisions, propertiesVectors.length)

	} 

	def findNumberCollisions(collisions: LinkedHashMap[Int, ArrayBuffer[Int]], numParticles: Int): Unit = {
		
		val collidedParticles = new ArrayBuffer[Int]()
		val sortedCollisions = LinkedHashMap(collisions.toSeq.sortBy(_._1):_*)
		
		val allCollisions = collisions.toSeq.flatMap(_._2).sortWith(_ < _)
//		println("All collisions " + allCollisions.mkString(", "))

		val numCollided = sortedCollisions.map{ case (time, particles) => 

			val collidingParticles = particles.distinct.diff(collidedParticles.distinct)

			if(collidingParticles.length >= 2) {
				collidedParticles ++= collidingParticles
				collidingParticles.length
			} else {
				0		
			}

		}.sum

		val sequence = (0 to 538).toArray
//		println("Sequence " + sequence.mkString(", "))
//		println("Missing " + sequence.diff(collidedParticles).mkString(", "))
//		println("Collided particles " + collidedParticles.sortWith(_ < _).mkString(", "))
//		println("Num particles " + numParticles)
//		println("Num collided " + collidedParticles.length)

		println("Solution " + (numParticles - numCollided))

	}
 
}
