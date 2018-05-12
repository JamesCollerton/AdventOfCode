package AdventOfCode.DaySeven

import AdventOfCode.Utils.ReadFileUtils

object DaySeven {

  def main(args: Array[String]): Unit = {
    val input = ReadFileUtils.readFileAsStringList("/AdventOfCode/DaySeven/DaySeven.txt")
    println(partOne(input).name)
    println(partTwo(input))
  }

  def partOne(input: List[String]): Node = {
    val baseNodeMap = generateBaseNodeMap(input)
    val completeNodeMap = generateCompleteNodeMap(input, baseNodeMap)
    findBottom(completeNodeMap)
  }

  def partTwo(input: List[String]): Int = {
    val bottom = partOne(input)
    val inputMap = generateInputMap(input)
    val tree = generateTree(inputMap.get(bottom.name).get, bottom.name, inputMap)
    tree.subNodes.map(sumTree).groupBy(_._2).minBy(_._1)._1
  }

  def sumTree(node: Node): (Boolean, Int) = {

    val subNodesResults = node.subNodes.map(n => (n, sumTree(n)))
    val foundValue = subNodesResults.filter(_._2._1)
    if(foundValue.length > 0) return (true, foundValue(0)._2._2)

    val subNodesTotals = subNodesResults.map(p => (p._1, p._2._2))
    val groupedNodes = subNodesTotals.groupBy(_._2)
    if(groupedNodes.size > 1) {
      val majorityValue = groupedNodes.maxBy(_._2.size)
      val minorityValue = groupedNodes.minBy(_._2.size)
      val adjustedValue = minorityValue._2(0)._1.value. + (majorityValue._1 - minorityValue._1)
      return (true, adjustedValue)
    }

    (false, node.value + subNodesTotals.map(_._2).sum)

  }

  def generateBaseNodeMap(input: List[String]): NodeMap = {
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

  def generateCompleteNodeMap(input: List[String], nodeMap: NodeMap): NodeMap = {
    NodeMap(input.map(line => {
        val splitLine = line.split("\\s+")
        val nodeKey = splitLine.take(1)(0)
        val subNodeKeys = splitLine.drop(3).map(str => str.replaceAll(",", ""))
        val node = nodeMap.get(nodeKey).get
        val subNodes = subNodeKeys.map(k => nodeMap.get(k).get).toVector
       (node.name, Node(node.name, node.value, subNodes))
    }).toMap)
  }

  def generateInputMap(input: List[String]): Map[String, String] = {
    input.map(line => (line.split("\\s+").take(1)(0), line)).toMap
  }

  def generateTree(line: String, nodeName: String, inputMap: Map[String, String]): Node = {
    val Array(currentName, dirtyCurrentWeight, dirtySubNodeNames @ _ *) = line.split("\\s+").filter(s => s != "->")
    val currentWeight = dirtyCurrentWeight.filter(c => c.isDigit).toInt
    val currentSubNodeNames = dirtySubNodeNames.map(s => s.replaceAll(",", ""))
    val currentSubNodes = currentSubNodeNames.map(s => generateTree(inputMap.get(s).get, s, inputMap)).toVector
    Node(currentName, currentWeight, currentSubNodes)
  }

  def findBottom(nodeMap: NodeMap): Node = {
    val allNodes = nodeMap.nodes.valuesIterator.toList
    val allNodeNames = allNodes.map(n => n.name)
    val supportedNodeNames = allNodes.flatMap(n => n.subNodes).map(n => n.name)
    val bottomNodeName = allNodeNames.filterNot(supportedNodeNames.toSet)(0)

    nodeMap.get(bottomNodeName).get
  }

}
