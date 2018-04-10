package AdventOfCode.DayOne

import org.scalatest.FunSuite

class DayOneTest extends FunSuite {

  test("Day One Test Part One 1122 equals 3") {
    assert(DayOne.partOne("1122") == 3)
  }

  test("Day One Test Part One 1111 equals 4") {
    assert(DayOne.partOne("1111") == 4)
  }

  test("Day One Test Part One 1234 equals 0") {
    assert(DayOne.partOne("1234") == 0)
  }

  test("Day One Test Part One 91212129 equals 9") {
    assert(DayOne.partOne("91212129") == 9)
  }

  test("Day One Test Part Two 1212 equals 6") {
    assert(DayOne.partTwo("1212") == 6)
  }

  test("Day One Test Part Two 1221 equals 0") {
    assert(DayOne.partTwo("1221") == 0)
  }

  test("Day One Test Part Two 123425 equals 4") {
    assert(DayOne.partTwo("123425") == 4)
  }

  test("Day One Test Part Two 123123 equals 12") {
    assert(DayOne.partTwo("123123") == 12)
  }

  test("Day One Test Part Two 12131415 equals 4") {
    assert(DayOne.partTwo("12131415") == 4)
  }

}
