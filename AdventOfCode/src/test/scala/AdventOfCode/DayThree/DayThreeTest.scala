package AdventOfCode.DayThree

import AdventOfCode.Utils.ReadFileUtils
import org.scalatest.FunSuite

class DayThreeTest extends FunSuite {

  test("Day Three Part One 1 is 0 steps") {
    assert(DayThree.partOne(1) == 0)
  }

  test("Day Three Part One 12 is 3 steps") {
    assert(DayThree.partOne(12) == 3)
  }

  test("Day Three Part One 23 is 2 steps") {
    assert(DayThree.partOne(23) == 2)
  }

  test("Day Three Part One 1024 is 31 steps") {
    assert(DayThree.partOne(1024) == 31)
  }

  test("Day Three getCoordinates 25 is (4, 0)") {
    assert(DayThree.getCoordinates(25) == ((4, 0), (2, 2)))
  }

  test("Day Three getCoordinates 21 is (0, 0)") {
    assert(DayThree.getCoordinates(21) == ((0, 0), (2, 2)))
  }

  test("Day Three getCoordinates 17 is (0, 4)") {
    assert(DayThree.getCoordinates(17) == ((0, 4), (2, 2)))
  }

  test("Day Three getCoordinates 13 is (4, 4)") {
    assert(DayThree.getCoordinates(13) == ((4, 4), (2, 2)))
  }

  test("Day Three getCoordinates 23 is (2, 0)") {
    assert(DayThree.getCoordinates(23) == ((2, 0), (2, 2)))
  }

  test("Day Three getCoordinates 19 is (0, 2)") {
    assert(DayThree.getCoordinates(19) == ((0, 2), (2, 2)))
  }

  test("Day Three getCoordinates 15 is (2, 4)") {
    assert(DayThree.getCoordinates(15) == ((2, 4), (2, 2)))
  }

  test("Day Three getCoordinates 11 is (4, 2)") {
    assert(DayThree.getCoordinates(11) == ((4, 2), (2, 2)))
  }
}
