package AdventOfCode.DaySeven

import org.scalatest.FunSuite

class DaySevenTest extends FunSuite {

  test("Read line to node fwft (72) -> ktlj, cntj, xhth") {
    assert(DaySeven.readLineToSingleNode("fwft (72) -> ktlj, cntj, xhth") == Node("fwft", 72, Vector()))
//    assertTrue(DaySeven.readLineToNode("fwft (72) -> ktlj, cntj, xhth") == Node("fwft", 72, Vector()))
  }

}
