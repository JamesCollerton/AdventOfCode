package AdventOfCode.DayFive

import org.scalatest.FunSuite

class DayFiveTest extends FunSuite {

  test("Day Five Part One 0 3 0 1 -3") {
    assert(DayFive.partOne(List(0, 3, 0, 1, -3)) == 5)
  }

  test("Day Five Part Two 0 3 0 1 -3") {
    assert(DayFive.partTwo(List(0, 3, 0, 1, -3)) == 10)
  }

}
