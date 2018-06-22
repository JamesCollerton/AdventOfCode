package AdventOfCode.DayEight

import AdventOfCode.DaySeven.{DaySeven, Node}
import org.scalatest.FunSuite

class DayEightTest extends FunSuite {

  test("Parse b inc 5 if a > 1") {
    assert(DayEight.parseLine("b inc 5 if a > 1") == DayEight.Instruction("b", "inc", 5, "a", ">", 1))
  }

}
