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

  def generateTree(input: List[String], nodeMap: NodeMap): Node = {

    val completeNodes = input.map(line => {
        val splitLine = line.split("\\s+")
        val nodeKey = splitLine.take(1)(0)
        val subNodeKeys = splitLine.drop(2)
        val node = nodeMap.get(nodeKey).get
        val subNodes = subNodeKeys.map(k => nodeMap.get(k).get).toVector
        Node(node.name, node.value, subNodes)
    })
    
  }

}
