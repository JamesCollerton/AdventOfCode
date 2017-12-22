import PropertiesVector._

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ArrayBuffer

object SolveEquations {

	def solveSingleEquation(a1: Double, a2: Double, v1: Double, v2: Double, p1: Double, p2: Double): (ArrayBuffer[Double], Boolean) = {

		// Solve quadratic equation
		if((a1 - a2) != 0.0) {
			val a = 0.5 * a1 - 0.5 * a2
			val b = 0.5 * (a1 - a2) + (v1 - v2)
			val c = p1 - p2
			val tPos = (-b + Math.sqrt(Math.pow(b,2) - 4 * a * c)) / (2 * a)
			val tNeg = (-b - Math.sqrt(Math.pow(b,2) - 4 * a * c)) / (2 * a)
			(ArrayBuffer(tPos, tNeg), false)
		// Solve linear equation
		} else if ((v1 - v2) != 0.0){
			val t = (p2 - p1)/ (v1 - v2)
			(ArrayBuffer(t), false)
		
		// See if in same plane
		} else if (p1 == p2){
			(new ArrayBuffer[Double], true)
		} else {
			(new ArrayBuffer[Double], false)
		}

	}

	def solveQuadratic(vecOne: PropertiesVector, vecTwo: PropertiesVector, collisions: LinkedHashMap[Int, ArrayBuffer[Int]], i: Int, j: Int): Unit =  {

		val (xCollisionTimes, xCollisionInfinite) = solveSingleEquation(vecOne.a.x, vecTwo.a.x, vecOne.v.x, vecTwo.v.x, vecOne.p.x, vecTwo.p.x)
		val (yCollisionTimes, yCollisionInfinite) = solveSingleEquation(vecOne.a.y, vecTwo.a.y, vecOne.v.y, vecTwo.v.y, vecOne.p.y, vecTwo.p.y)
		val (zCollisionTimes, zCollisionInfinite) = solveSingleEquation(vecOne.a.z, vecTwo.a.z, vecOne.v.z, vecTwo.v.z, vecOne.p.z, vecTwo.p.z)

		val intersectTimes = if(xCollisionInfinite && yCollisionInfinite && zCollisionInfinite) {
			// Same spot, don't move	
			ArrayBuffer(0.0)
		} else if(xCollisionInfinite && yCollisionInfinite) {
			// Only move on z axis
			zCollisionTimes
		} else if(xCollisionInfinite && zCollisionInfinite) {
			// Only move on y axis
			yCollisionTimes
		} else if(yCollisionInfinite && zCollisionInfinite) {
			// Only move on x axis
			xCollisionTimes
		} else if(xCollisionInfinite) {
			// Move on y and z axes
			yCollisionTimes.intersect(zCollisionTimes)
		} else if(yCollisionInfinite) {
			// Move on x and z axes
			xCollisionTimes.intersect(zCollisionTimes)
		} else if(zCollisionInfinite) {
			// Move on x and y axes
			xCollisionTimes.intersect(yCollisionTimes)
		} else {
			// Moving on all axes
			xCollisionTimes.intersect(yCollisionTimes.intersect(zCollisionTimes))
		}

		if(intersectTimes.length > 0) {
			intersectTimes.foreach(t => {
				if(!collisions.contains(t.toInt)) collisions(t.toInt) = new ArrayBuffer[Int]
				collisions(t.toInt) += i
				collisions(t.toInt) += j
			})
		}

	}

}
