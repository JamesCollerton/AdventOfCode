package AdventOfCode.DayFour

import org.scalatest.FunSuite

class DayFourTest extends FunSuite {

  test("Part One valid passphrase") {
    assert(DayFour.partOne(List("aa bb cc dd ee")) == 1)
  }

  test("Part One valid passphrase aa/ aaa") {
    assert(DayFour.partOne(List("aa bb cc dd aaa")) == 1)
  }

  test("Part One invalid passphrase") {
    assert(DayFour.partOne(List("aa bb cc dd aa")) == 0)
  }

  test("stringMapCreator aaa = a -> 3") {
    assert(DayFour.stringMapCreator("aaa") == Map('a' -> 3))
  }

  test("stringMapCreator abcd = a -> 1, b -> 1, c -> 1, d -> 1") {
    assert(DayFour.stringMapCreator("abcd") == Map('a' -> 1, 'b' -> 1, 'c' -> 1, 'd' -> 1))
  }

  test("Part Two valid passphrase abcde fghij") {
    assert(DayFour.partTwo(List("abcde fghij")) == 1)
  }

  test("Part Two invalid passphrase abcde xyz ecdab") {
    assert(DayFour.partTwo(List("abcde xyz ecdab")) == 0)
  }

  test("Part Two valid passphrase a ab abc abd abf abj") {
    assert(DayFour.partTwo(List("a ab abc abd abf abj")) == 1)
  }

  test("Part Two valid passphrase iiii oiii ooii oooi oooo") {
    assert(DayFour.partTwo(List("iiii oiii ooii oooi oooo")) == 1)
  }

  test("Part Two invalid passphrase oiii ioii iioi iiio") {
    assert(DayFour.partTwo(List("oiii ioii iioi iiio")) == 0)
  }

}
