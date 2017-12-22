package ThreeDVector

class ThreeDVector[A](val x: A, val y: A, val z: A) {

	def printVector(): Unit = {
		println("" + x + ", " + y + ", " + z)
	}

}

