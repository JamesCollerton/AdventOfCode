package AdventOfCode.Utils

import org.scalatest.FunSuite

class ReadFileUtilsTest extends FunSuite {

  test("Read file as string") {
    assert(ReadFileUtils.readFileAsString("/AdventOfCode/Utils/ReadFileAsString.txt") == "1234abcde!?!?")
  }

  test("Read file as string list") {
    assert(ReadFileUtils.readFileAsStringList("/AdventOfCode/Utils/ReadFileAsStringList.txt") == List("1234", "abcde", "?!?"))
  }

  test("Read file as int list list") {
    assert(ReadFileUtils.readFileAsIntListList("/AdventOfCode/Utils/ReadFileAsIntListList.txt") == List(List(1,2,3,4), List(5,6,7), List(8,9,10)))
  }

  test("Read file as int list") {
    assert(ReadFileUtils.readFileAsIntList("/AdventOfCode/Utils/ReadFileAsIntList.txt") == List(0, 3, 0, 1, -3))
  }

  test("Read file as int") {
    assert(ReadFileUtils.readFileAsInt("/AdventOfCode/Utils/ReadFileAsInt.txt") == 1453)
  }

}
