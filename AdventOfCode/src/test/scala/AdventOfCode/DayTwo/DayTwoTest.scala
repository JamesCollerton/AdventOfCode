package AdventOfCode.DayTwo

import AdventOfCode.Utils.ReadFileUtils
import org.scalatest.FunSuite

class DayTwoTest extends FunSuite {

  test("Day Two Test Part One Sample Input equals 18") {
    val input = ReadFileUtils.readFileAsIntListList("/AdventOfCode/DayTwo/DayTwoPartOneTest.txt")
    assert(DayTwo.partOne(input) == 18)
  }

  test("Day Two Test Part Two Sample Input equals 9") {
    val input = ReadFileUtils.readFileAsIntListList("/AdventOfCode/DayTwo/DayTwoPartTwoTest.txt")
    assert(DayTwo.partTwo(input) == 9)
  }

}
