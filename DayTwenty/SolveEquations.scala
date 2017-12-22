import CoefficientCalculator._
import PropertiesVector._

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ArrayBuffer

object SolveEquations {

	def solveSingleEquation(a1: Double, a2: Double, v1: Double, v2: Double, p1: Double, p2: Double): ArrayBuffer[Double] = {
		// Solve quadratic equation
		if(a1 != 0 || a2 != 0) {
			val a = 0.5 * a1 - 0.5 * a2
			val b = 0.5 * (a1 - a2) + (v1 - v2)
			val c = p1 - p2
			val tPos = (-b + Math.sqrt(Math.pow(b,2) - 4 * a * c)) / (2 * a)
			val tNeg = (-b - Math.sqrt(Math.pow(b,2) - 4 * a * c)) / (2 * a)
			ArrayBuffer(tPos, tNeg)
		// Solve linear equation
		} else {
			val t = (p2 - p1)/ (v2 - v1)
			ArrayBuffer(t)
		}
	}

	def solveQuadratic(vecOne: PropertiesVector, vecTwo: PropertiesVector, collisions: LinkedHashMap[Int, ArrayBuffer[Int]], i: Int, j: Int): Unit =  {

		def aFunc(a1: Double, a2: Double, v1: Double, v2: Double, p1: Double, p2: Double): Double = {
			0.5 * a1 - 0.5 * a2
		}
		val a = CoefficientCalculator.calculateCoefficient(vecOne, vecTwo)(aFunc)
	
		def bFunc(a1: Double, a2: Double, v1: Double, v2: Double, p1: Double, p2: Double): Double = {
			0.5 * (a1 - a2) + (v1 - v2)
		}
		val b = CoefficientCalculator.calculateCoefficient(vecOne, vecTwo)(bFunc)

		def cFunc(a1: Double, a2: Double, v1: Double, v2: Double, p1: Double, p2: Double): Double = {
			p1 - p2
		}
		val c = CoefficientCalculator.calculateCoefficient(vecOne, vecTwo)(cFunc)

		def posSolnFunc(a: Double, b: Double, c: Double): Double = {
			(-b + Math.sqrt(Math.pow(b, 2) - 4 * a * c)) / (2 * a)
		}
		val solnPos = CoefficientCalculator.calculateSolution(a, b, c)(posSolnFunc)

		def negSolnFunc(a: Double, b: Double, c: Double): Double = {
			(-b - Math.sqrt(Math.pow(b, 2) - 4 * a * c)) / (2 * a)
		}
		val solnNeg = CoefficientCalculator.calculateSolution(a, b, c)(negSolnFunc)
		
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
