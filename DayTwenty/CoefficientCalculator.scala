import PropertiesVector._
import ThreeDVector._

object CoefficientCalculator {
	
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

}
