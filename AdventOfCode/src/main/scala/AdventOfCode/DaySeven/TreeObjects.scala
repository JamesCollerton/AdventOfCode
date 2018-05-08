package AdventOfCode.DaySeven

case class Node(name: String, value: Int, subNodes: Vector[Node])

case class NodeMap(nodes: Map[String, Node]) {

  def put(key: String, value: Node): NodeMap = {
    NodeMap(nodes + (key -> value))
  }

  def get(key: String) : Option[Node] = {
    nodes.get(key)
  }

}
