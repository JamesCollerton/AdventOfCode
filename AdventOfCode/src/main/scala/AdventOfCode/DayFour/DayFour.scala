package AdventOfCode.DayFour

import AdventOfCode.Utils.ReadFileUtils

object DayFour {

  def main(args: Array[String]): Unit = {
    val input = ReadFileUtils.readFileAsStringList("/AdventOfCode/DayFour/DayFour.txt")
    println(partOne(input))
    println(partTwo(input))
  }

  def partOne(input: List[String]): Int = {
    passwordMapper(input)(identity)
  }

  def partTwo(input: List[String]): Int = {
    passwordMapper(input)(stringMapCreator)
  }

  def passwordMapper[A](input: List[String])(f: String => A): Int = {
    input.count(p => p.split(" ").length == p.split(" ").map(f).distinct.length)
  }

  def stringMapCreator(string: String): Map[Char, Int] = {
    string.groupBy(identity).mapValues(_.size)
  }

}
