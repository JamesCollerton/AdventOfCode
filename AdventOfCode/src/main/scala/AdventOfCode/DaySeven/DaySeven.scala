package AdventOfCode.DaySeven

object DaySeven {

  def main(args: Array[String]): Unit = {
    readLineToSingleNode("A B", NodeMap(Map()))
  }

  def readLineToSingleNode(line: String, nodeMap: NodeMap): Node = {
    val Array(name, weight) = line.split("\\s+").take(2)
    println(name + "/" + weight)
    Node("", 0, Vector())
  }

}
