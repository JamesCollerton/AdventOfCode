package AdventOfCode.DayEight

import org.scalatest.FunSuite

class DayEightTest extends FunSuite {

  test("Parse b inc 5 if a > 1") {
    assert(DayEight.parseLine("b inc 5 if a > 1") == DayEight.Instruction("b", "inc", 5, "a", ">", 1))
  }

  test("Create registers a, b") {
    val instructionOne = DayEight.Instruction("b", "inc", 5, "a", ">", 1)
    val instructionTwo = DayEight.Instruction("a", "inc", 5, "b", ">", 1)

    val registers = Map("a" -> 0, "b" -> 0)
    assert(DayEight.createRegisters(List(instructionOne, instructionTwo)) == registers)
  }

  test("Should update, false") {
    val instructionOne = DayEight.Instruction("b", "inc", 5, "a", ">=", 1)
    val register = Map("a" -> 0)
    assert(instructionOne.shouldUpdate(register) == false)
  }

  test("Should update, true") {
    val instructionOne = DayEight.Instruction("b", "inc", 5, "a", "<", 1)
    val register = Map("a" -> 0)
    assert(instructionOne.shouldUpdate(register) == true)
  }

  test("Get new value, inc, increases correctly") {
    val instructionOne = DayEight.Instruction("a", "inc", 5, "a", "<", 1)
    val register = Map("a" -> -5)
    assert(instructionOne.getNewValue(register) == 0)
  }

  test("Get new value, dec, decreases correctly") {
    val instructionOne = DayEight.Instruction("a", "dec", 5, "a", "<", 1)
    val register = Map("a" -> 5)
    assert(instructionOne.getNewValue(register) == 0)
  }

  test("Test part one") {
    assert(DayEight.partOne("/AdventOfCode/DayEight/DayEightTest.txt") == 1)
  }

}
