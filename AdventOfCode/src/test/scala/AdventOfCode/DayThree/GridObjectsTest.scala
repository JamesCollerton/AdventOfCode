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

  test("Convert coordinates first quadrant correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(1, 1)

    assert(coordinates.convert(grid) == Coordinates(2, 0))
  }

  test("Convert coordinates second quadrant correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(-1, 1)

    assert(coordinates.convert(grid) == Coordinates(0, 0))
  }

  test("Convert coordinates third quadrant correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(-1, -1)

    assert(coordinates.convert(grid) == Coordinates(0, 2))
  }

  test("Convert coordinates fourth quadrant correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(1, -1)

    assert(coordinates.convert(grid) == Coordinates(2, 2))
  }

  test("Check for append left correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(0, 1)

    assert(grid.checkForAppend(coordinates) == grid.appendZeroBorder())
  }

  test("Check for append right correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(2, 1)

    assert(grid.checkForAppend(coordinates) == grid.appendZeroBorder())
  }

  test("Check for append top correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(1, 0)

    assert(grid.checkForAppend(coordinates) == grid.appendZeroBorder())
  }

  test("Check for append bottom correct") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(1, 2)

    assert(grid.checkForAppend(coordinates) == grid.appendZeroBorder())
  }

  test("Replace at (0,0)") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val newGrid = Grid(List(List(100, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val coordinates = Coordinates(0, 0)
    val toReplace = 100

    assert(grid.replace(coordinates, toReplace) == newGrid)
  }

  test("Replace at (2, 2)") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val newGrid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 100)))
    val coordinates = Coordinates(2, 2)
    val toReplace = 100

    assert(grid.replace(coordinates, toReplace) == newGrid)
  }

  test("Calculate next grid, (0, 0)") {
    val grid = Grid(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val newGrid = Grid(List(List(1, 2, 3), List(4, 40, 6), List(7, 8, 9)))
    val coordinates = Coordinates(0, 0)

    assert(grid.calculateNextGrid(coordinates) == newGrid)
  }

  test("Calculate next grid, (-1, 1)") {
    val grid = Grid(List(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10), List(11, 12, 13, 14, 15), List(16, 17, 18, 19, 20), List(21, 22, 23, 24, 25)))
    val newGrid = Grid(List(List(1, 2, 3, 4, 5), List(6, 56, 8, 9, 10), List(11, 12, 13, 14, 15), List(16, 17, 18, 19, 20), List(21, 22, 23, 24, 25)))
    val coordinates = Coordinates(-1, 1)

    assert(grid.calculateNextGrid(coordinates) == newGrid)
  }

  test("Move side, first step") {
    val grid = Grid(List(List(0, 0, 0), List(0, 1, 0), List(0, 0, 0)))
    val coordinates = Coordinates(1, 0)
    val direction = Direction.NORTH
    val position = Position(grid, direction, coordinates)

    val newGrid = Grid(List(List(0, 0, 0), List(0, 1, 1), List(0, 0, 0)))
    val newCoordinates = Coordinates(1, 0)
    val newDirection = Direction.WEST
    val newPosition = Position(newGrid, newDirection, newCoordinates)

    assert(Mover.moveSide(position, 0) == newPosition)
  }

  test("Move side, top side") {
    val grid = Grid(List(List(0, 0, 0), List(0, 1, 1), List(0, 0, 0)))
    val coordinates = Coordinates(1, 1)
    val direction = Direction.WEST
    val position = Position(grid, direction, coordinates)

    val newGrid = Grid(List(List(5, 4, 2), List(0, 1, 1), List(0, 0, 0)))
    val newCoordinates = Coordinates(-1, 1)
    val newDirection = Direction.SOUTH
    val newPosition = Position(newGrid, newDirection, newCoordinates)

    assert(Mover.moveSide(position, 2) == newPosition)
  }

  test("Move side, left side") {
    val grid = Grid(List(List(5, 4, 2), List(0, 1, 1), List(0, 0, 0)))
    val coordinates = Coordinates(-1, 1)
    val direction = Direction.SOUTH
    val position = Position(grid, direction, coordinates)

    val newGrid = Grid(List(List(5, 4, 2), List(10, 1, 1), List(11, 0, 0)))
    val newCoordinates = Coordinates(-1, -1)
    val newDirection = Direction.EAST
    val newPosition = Position(newGrid, newDirection, newCoordinates)

    assert(Mover.moveSide(position, 2) == newPosition)
  }

  test("Move side, bottom side") {
    val grid = Grid(List(List(5, 4, 2), List(10, 1, 1), List(11, 0, 0)))
    val coordinates = Coordinates(-1, -1)
    val direction = Direction.EAST
    val position = Position(grid, direction, coordinates)

    val newGrid = Grid(List(List(5, 4, 2), List(10, 1, 1), List(11, 23, 25)))
    val newCoordinates = Coordinates(1, -1)
    val newDirection = Direction.NORTH
    val newPosition = Position(newGrid, newDirection, newCoordinates)

    assert(Mover.moveSide(position, 2) == newPosition)
  }

  test("Make circle, 4x4") {
    val grid = Grid(List(List(5, 4, 2), List(10, 1, 1), List(11, 23, 25))).appendZeroBorder()
    val coordinates = Coordinates(2, -1)
    val direction = Direction.NORTH
    val position = Position(grid, direction, coordinates)

    val newGrid = Grid(List(List(0, 0, 0, 0, 0, 0, 0), List(0, 147, 142, 133, 122, 59, 0), List(0, 304, 5, 4, 2, 57, 0), List(0, 330, 10, 1, 1, 54, 0), List(0, 351, 11, 23, 25, 26, 0), List(0, 362, 747, 806, 880, 931, 957), List(0, 0, 0, 0, 0, 0, 0)))
    val newCoordinates = Coordinates(3, -2)
    val newDirection = Direction.NORTH
    val newPosition = Position(newGrid, newDirection, newCoordinates)

    assert(Mover.makeCircle(position, 3) == newPosition)
  }

}
