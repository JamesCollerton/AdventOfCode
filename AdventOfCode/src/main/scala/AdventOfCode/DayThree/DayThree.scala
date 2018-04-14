package AdventOfCode.DayThree

object DayThree {

  def main(args: Array[String]): Unit = {
    println(partOne(325489))
  }

  def partOne(input: Int): Int = {
    if(input == 1) return 0
    val ((x, y), (centreX, centreY)) = getCoordinates(input)
    Math.abs(x - centreX) + Math.abs(y - centreY)
  }

  def getCoordinates(input: Int): ((Int, Int), (Int, Int)) = {

    val root = Math.sqrt(input).toInt

    val upperBoundRoot = if((input == root * root) && input % 2 == 1) {
      root
    }else if(root % 2 == 1) {
      root + 2
    } else {
      root + 1
    }

    val upperBoundSquare = upperBoundRoot * upperBoundRoot
    val sideLength = upperBoundRoot - 1
    val centre = if(sideLength % 2 == 0) sideLength / 2 else sideLength / 2 + 1
    val sideInt = (upperBoundSquare - input) / (sideLength)
    val cornerDistance = (upperBoundSquare - sideInt * sideLength) - input

    val coordinates = if(sideInt == 0) {
      (sideLength - cornerDistance, 0)
    } else if(sideInt == 1) {
      (0, cornerDistance)
    } else if(sideInt == 2) {
      (cornerDistance, sideLength)
    } else {
      (sideLength, sideLength - cornerDistance)
    }

    (coordinates, (centre, centre))
  }

  def partTwo(input: Int): Int = {
    -1
  }

  def makeGrid(input: Int): Int = {
    val startGrid = Grid(List(List(0, 0, 0), List(0, 1, 0), List(0, 0, 0)))
    val startPosition = Coordinates(0, 0)
    val startDirection = Direction.EAST
//    val direction
    0
  }

}
