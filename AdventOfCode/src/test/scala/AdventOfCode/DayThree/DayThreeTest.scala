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

}
