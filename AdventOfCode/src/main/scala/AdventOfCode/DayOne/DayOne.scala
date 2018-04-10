package AdventOfCode.DayOne

import AdventOfCode.Utils

object DayOne {

  def main(args: Array[String]): Unit = {

    val input = Utils.readFileAsList("/DayOne.txt")

    val partOneAns = partOne(input(0))
    val partTwoAns = partTwo(input(0))

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
