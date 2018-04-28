package AdventOfCode.DaySix

import org.scalatest.FunSuite

class DaySixTest extends FunSuite {

  test("Find max repeated value test") {
    assert(DaySix.findMaxIndex(List(2, 1, 2)) == 0)
  }

  test("Redistribute blocks test") {
    assert(DaySix.redistributeBlocks(List(0, 2, 0, 0), 3, 7) == List(2, 4, 1, 2))
  }

  test("Day Six Part One") {
    assert(DaySix.partOne(List(0, 2, 7, 0)) == 5)
  }

}
