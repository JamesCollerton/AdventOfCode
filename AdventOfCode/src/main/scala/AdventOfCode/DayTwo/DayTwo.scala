package AdventOfCode.DayTwo

import AdventOfCode.Utils.ReadFileUtils

object DayTwo {

  def main(args: Array[String]): Unit = {

    val input = ReadFileUtils.readFileAsIntListList("/AdventOfCode/DayTwo/DayTwo.txt")

    List(partOne(input), partTwo(input)).zipWithIndex.foreach{ case (ans, i) => println(s"Part $i answer: $ans") }

  }

  def partOne(input: List[List[Int]]): Int = {
    input.map(line => line.max - line.min).sum
  }

  def partTwo(input: List[List[Int]]): Int = {
    input.map(line =>
      (for {
        i <- line
        j <- line
        if i % j == 0 && i != j
      } yield i / j).sum
    ).sum
  }

}
