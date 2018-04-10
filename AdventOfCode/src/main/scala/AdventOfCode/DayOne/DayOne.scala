package AdventOfCode.DayOne

import AdventOfCode.Utils

object DayOne {

  def main(args: Array[String]): Unit = {

    val input = Utils.readFileAsString("/AdventOfCode/DayOne/DayOne.txt")

    val (partOneAns, partTwoAns) = (partOne(input), partTwo(input))

    println(s"Part one: $partOneAns")
    println(s"Part two: $partTwoAns")

  }

  def partOne(input: String): Int = {
    1
  }

  def partTwo(input: String): Int = {
    0
  }

}
