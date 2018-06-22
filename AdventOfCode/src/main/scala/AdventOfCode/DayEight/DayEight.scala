package AdventOfCode.DayEight

import AdventOfCode.Utils.ReadFileUtils

object DayEight {

  def main(args: Array[String]): Unit = {

    parseInput("/AdventOfCode/DayFive/DayFive.txt")
    
  }

  def parseInput(filename: String): Unit = {
    val input = ReadFileUtils.readFileAsStringList(filename)
  }

  def parseLine(line: String): Instruction = {
    val splitLine = line.split("\\s+")
    Instruction(splitLine(0), splitLine(1), splitLine(2).toInt, splitLine(4), splitLine(5), splitLine(6).toInt)
  }

  case class Instruction(regToChange: String, action: String, checkAmount: Int, regToCheck: String, operation: String, changeAmount: Int)

}
