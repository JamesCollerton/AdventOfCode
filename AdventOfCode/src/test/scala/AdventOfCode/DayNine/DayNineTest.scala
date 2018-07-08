package AdventOfCode.DayNine

import org.scalatest.FunSuite

class DayNineTest extends FunSuite {

  test("Part one, {}, score of 1") {
    assert(DayNine.countGroups("{}".split(""), false, 0) == 1)
  }

  test("Part one, {{{}}}, score of 6") {
    assert(DayNine.countGroups("{{{}}}".split(""), false, 0) == 6)
  }

  test("Part one, {{},{}}, score of 5") {
    assert(DayNine.countGroups("{{},{}}".split(""), false, 0) == 5)
  }

  test("Part one, {{{},{},{{}}}}, score of 16") {
    assert(DayNine.countGroups("{{{},{},{{}}}}".split(""), false, 0) == 16)
  }

  test("Part one, {<a>,<a>,<a>,<a>}, score of 1") {
    assert(DayNine.countGroups("{<a>,<a>,<a>,<a>}".split(""), false, 0) == 1)
  }

  test("Part one, {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 9") {
    assert(DayNine.countGroups("{{<ab>},{<ab>},{<ab>},{<ab>}}".split(""), false, 0) == 9)
  }

  test("Part one, {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 9") {
    assert(DayNine.countGroups("{{<!!>},{<!!>},{<!!>},{<!!>}}".split(""), false, 0) == 9)
  }

  test("Part one, {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 3") {
    assert(DayNine.countGroups("{{<a!>},{<a!>},{<a!>},{<ab>}}".split(""), false, 0) == 3)
  }

  test("Part two, <>, score of 0") {
    assert(DayNine.countGarbage("<>".split(""), false) == 0)
  }

  test("Part two, <random characters>, score of 17") {
    assert(DayNine.countGarbage("<random characters>".split(""), false) == 17)
  }

  test("Part two, <<<<>, score of 3") {
    assert(DayNine.countGarbage("<<<<>".split(""), false) == 3)
  }

  test("Part two, <{!>}>, score of 2") {
    assert(DayNine.countGarbage("<{!>}>".split(""), false) == 2)
  }

  test("Part two, <!!>, score of 0") {
    assert(DayNine.countGarbage("<!!>".split(""), false) == 0)
  }

  test("Part two, <!!!>>, score of 0") {
    assert(DayNine.countGarbage("<!!!>>".split(""), false) == 0)
  }

  test("Part two, <{o\"i!a,<{i<a>, score of 10") {
    assert(DayNine.countGarbage("<{o\"i!a,<{i<a>".split(""), false) == 10)
  }

}
