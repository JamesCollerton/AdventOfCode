package AdventOfCode.DayThree

object DayThree {

  def main(args: Array[String]): Unit = {

  }

  def partOne(input: Int): Int = {
    if(input == 1) return 1

    getCoordinates(input)

    -1
  }

  def getCoordinates(input: Int): Int = {
    val upperBoundSquare = Math.sqrt(input).toInt + 1
    if(input > upperBoundSquare - 1
    val remainder = (upperBoundSquare - input) / (upperBoundSquare - 1)

  }

  def partTwo(input: Int): Int = {
    -1
  }

}
