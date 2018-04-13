package AdventOfCode.DayThree

object DayThree {

  def main(args: Array[String]): Unit = {

  }

  def partOne(input: Int): Int = {
    if(input == 1) return 1

    getCoordinates(input)

    -1
  }

  def getCoordinates(input: Int): (Int, Int) = {

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

    coordinates
  }

  def partTwo(input: Int): Int = {
    -1
  }

}
