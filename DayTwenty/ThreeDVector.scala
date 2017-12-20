package ThreeDVector

class ThreeDVector(val x: Int, val y: Int, val z: Int)

class PropertiesVector(val p: ThreeDVector, val v: ThreeDVector, val a: ThreeDVector) {

	def getAcceleration(): Int = {
		Math.abs(a.x) + Math.abs(a.y) + Math.abs(a.z)
	}

	def getPositionDistance(): Int = {
		Math.abs(p.x) + Math.abs(p.y) + Math.abs(p.z)
	}

}
