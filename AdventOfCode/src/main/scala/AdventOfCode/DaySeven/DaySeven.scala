package AdventOfCode.DaySeven

object DaySeven {

  def main(args: Array[String]): Unit = {
    println("Hello, world.")
  }

  case class Node(name: String, value: Int, subNodes: Vector[Node])

  def readLineToNode(line: String): Node = {
    Node("Test", 1, Vector())
  }

}
