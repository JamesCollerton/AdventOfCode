package ThreeDVector

class ThreeDVector[A](val x: A, val y: A, val z: A)

class PropertiesVector(val p: ThreeDVector[Double], val v: ThreeDVector[Double], val a: ThreeDVector[Double]) {

	def getAcceleration(): Double = {
		Math.abs(a.x) + Math.abs(a.y) + Math.abs(a.z)
	}

	def getPositionDistance(): Double = {
		Math.abs(p.x) + Math.abs(p.y) + Math.abs(p.z)
	}

}
