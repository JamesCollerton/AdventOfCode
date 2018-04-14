package AdventOfCode.DayThree

import org.scalatest.FunSuite

class GridObjectsTest extends FunSuite {

  test("Direction Enum North correct") {
    assert(Direction.NORTH.nextDirection == Direction.WEST)
    assert(Direction.NORTH.increment == Coordinates(0, 1))
  }

  test("Direction Enum West correct") {
    assert(Direction.WEST.nextDirection == Direction.SOUTH)
    assert(Direction.WEST.increment == Coordinates(-1, 0))
  }

  test("Direction Enum South correct") {
    assert(Direction.SOUTH.nextDirection == Direction.EAST)
    assert(Direction.SOUTH.increment == Coordinates(0, -1))
  }

  test("Direction Enum East correct") {
    assert(Direction.EAST.nextDirection == Direction.NORTH)
    assert(Direction.EAST.increment == Coordinates(1, 0))
  }

}
