import CoefficientCalculator._
import PropertiesVector._

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ArrayBuffer

object SolveEquations {

	def solveSingleEquation(a1: Double, a2: Double, v1: Double, v2: Double, p1: Double, p2: Double): Stream[Double] = {
		println("a1 " + a1 + ", a2 " + a2 + ", v1 " + v1 + ", v2 " + v2 + ", p1 " + p1 + ", p2 " + p2)
		// Solve quadratic equation
		if(a1 != 0.0 || a2 != 0.0) {
			val a = 0.5 * a1 - 0.5 * a2
			val b = 0.5 * (a1 - a2) + (v1 - v2)
			val c = p1 - p2
			val tPos = (-b + Math.sqrt(Math.pow(b,2) - 4 * a * c)) / (2 * a)
			val tNeg = (-b - Math.sqrt(Math.pow(b,2) - 4 * a * c)) / (2 * a)
			tPos #:: tNeg #:: Stream.empty
		// Solve linear equation
		} else if ((v1 - v2) != 0.0){
			println("In linear")
			val t = (p2 - p1)/ (v1 - v2)
			t #:: Stream.empty
		// See if in same plane
		} else if (p1 == p2){
			println("In stationary")
			println("Here")
			val infiniteTimeStream: Stream[Double] = {
  				def loop(v: Double): Stream[Double] = v #:: loop(v + 1)
  				loop(0)
			}		
			infiniteTimeStream
		} else {
			Stream.Empty
		}

	}

	def solveQuadratic(vecOne: PropertiesVector, vecTwo: PropertiesVector, collisions: LinkedHashMap[Int, ArrayBuffer[Int]], i: Int, j: Int): Unit =  {

		val xCollisionTimes = solveSingleEquation(vecOne.a.x, vecTwo.a.x, vecOne.v.x, vecTwo.v.x, vecOne.p.x, vecTwo.p.x)
		val yCollisionTimes = solveSingleEquation(vecOne.a.y, vecTwo.a.y, vecOne.v.y, vecTwo.v.y, vecOne.p.y, vecTwo.p.y)
		val zCollisionTimes = solveSingleEquation(vecOne.a.z, vecTwo.a.z, vecOne.v.z, vecTwo.v.z, vecOne.p.z, vecTwo.p.z)

		println()
		//println(yCollisionTimes.length)
		println("X " + xCollisionTimes.mkString(", "))
		println("Y " + yCollisionTimes.mkString(", "))
		println("Z " + zCollisionTimes.mkString(", "))

		// If we intersect on all three planes at once
		val intersectTimes = xCollisionTimes.intersect(yCollisionTimes.intersect(zCollisionTimes)) 
		if(intersectTimes.length > 0) {
			intersectTimes.foreach(t => {
				println(t)
				collisions(t.toInt) += i
				collisions(t.toInt) += j
			})
		}

	}

}
