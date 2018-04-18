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

  test("Grid Append Zeros correct") {
    val gridOriginal = Grid(List(List(0, 0, 0), List(0, 1, 0), List(0, 0, 0)))
    val gridExpected = Grid(List(List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 1, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0)))
    assert(gridOriginal.appendZeroBorder() == gridExpected)
  }

  test("Sum surrounding points correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(1, 1)
    assert(grid.sumSurroundingPoints(coordinates) == 40)
  }

  test("Convert coordinates correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(1, 1)

    assert(grid.sumSurroundingPoints(coordinates) == 40)
  }

}
