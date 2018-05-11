package AdventOfCode.DaySeven

import org.scalatest.FunSuite

class DaySevenTest extends FunSuite {

  test("Read line to node fwft (72) -> ktlj, cntj, xhth") {
    assert(DaySeven.readLineToSingleNode("fwft (72) -> ktlj, cntj, xhth") == Node("fwft", 72, Vector()))
  }

  test("Generate node map fwft (72) -> ktlj, cntj, xhth") {
    assert(DaySeven.generateBaseNodeMap(List("fwft (72) -> ktlj, cntj, xhth")) == NodeMap(Map("fwft" -> Node("fwft", 72, Vector()))))
  }

  test("Generate node map fwft (72) -> ktlj, cntj, xhth") {
    assert(DaySeven.generateBaseNodeMap(List("fwft (72) -> ktlj, cntj, xhth")) == NodeMap(Map("fwft" -> Node("fwft", 72, Vector()))))
  }

}
