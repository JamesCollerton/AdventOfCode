package PropertiesVector

import ThreeDVector._

class PropertiesVector(val p: ThreeDVector[Double], val v: ThreeDVector[Double], val a: ThreeDVector[Double]) {

	def getAcceleration(): Double = {
		Math.abs(a.x) + Math.abs(a.y) + Math.abs(a.z)
	}

	def getPositionDistance(): Double = {
		Math.abs(p.x) + Math.abs(p.y) + Math.abs(p.z)
	}

	def printVector(): Unit = {
		print("p ")
		p.printVector()
		print("v ")
		v.printVector()
		print("a ")
		a.printVector()
	}

}

