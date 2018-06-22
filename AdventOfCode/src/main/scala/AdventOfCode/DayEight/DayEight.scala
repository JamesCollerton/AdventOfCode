package AdventOfCode.DayEight

import AdventOfCode.Utils.ReadFileUtils

object DayEight {

  def main(args: Array[String]): Unit = {

    parseInput("/AdventOfCode/DayFive/DayFive.txt")
    
  }

  def parseInput(filename: String): Unit = {
    val input = ReadFileUtils.readFileAsStringList(filename)
  }

  case class Instruction(regToChange: String, action: String, checkAmount: Int, regToCheck: String, operation: String, changeAmount: Int)

}
