package AdventOfCode.DaySeven

import AdventOfCode.Utils.ReadFileUtils

object DaySeven {

  def main(args: Array[String]): Unit = {
    val input = ReadFileUtils.readFileAsStringList("/AdventOfCode/DaySeven/DaySeven.txt")
    partOne(input)
  }

  def partOne(input: List[String]): Unit = {
    val nodeMap = generateNodeMap(input)
  }

  def generateNodeMap(input: List[String]): NodeMap = {
    NodeMap((for {
      inputLine <- input
      node = readLineToSingleNode(inputLine)
    } yield (node.name, node)).toMap)
  }

  def readLineToSingleNode(line: String): Node = {
    val Array(name, weight) = line.split("\\s+").take(2)
    val cleanWeight = weight.filter(c => c.isDigit).toInt
    Node(name, cleanWeight, Vector())
  }

}
