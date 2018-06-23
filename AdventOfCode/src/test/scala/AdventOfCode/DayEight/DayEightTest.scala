package AdventOfCode.DayEight

import org.scalatest.FunSuite

class DayEightTest extends FunSuite {

  test("Parse b inc 5 if a > 1") {
    assert(DayEight.parseLine("b inc 5 if a > 1") == DayEight.Instruction("b", "inc", 5, "a", ">", 1))
  }

  test("Create registers a, b") {
    val instructionOne = DayEight.Instruction("b", "inc", 5, "a", ">", 1)
    val instructionTwo = DayEight.Instruction("a", "inc", 5, "b", ">", 1)

    val registerOne = DayEight.Register("a", 0)
    val registerTwo = DayEight.Register("b", 0)
    assert(DayEight.createRegisters(List(instructionOne, instructionTwo)).sortBy(reg => reg.name) == List(registerOne, registerTwo).sortBy(reg => reg.name))
  }

}
