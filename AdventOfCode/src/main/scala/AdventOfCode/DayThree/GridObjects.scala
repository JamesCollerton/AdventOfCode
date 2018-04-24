package AdventOfCode.DayThree

import AdventOfCode.DayThree.Direction.Direction

case class Position(grid: Grid, direction: Direction, coordinates: Coordinates)

case class Coordinates(x: Int, y: Int) {

  def convert(grid: Grid): Coordinates = {
    Coordinates((grid.size / 2) + x, (grid.size / 2) - y)
  }

}

case class Grid(grid: List[List[Int]]) {

  def calculateNextGrid(coordinates: Coordinates): Grid = {

    // Convert the coordinates from (x, y) to array coordinates.
    val convertedCoordinates = coordinates.convert(this)

    // If we are about to walk off the edge then add a border of zeros
    val newGrid = checkForAppend(convertedCoordinates);

    // Sum all of the surrounding areas
    val surroundingSum = newGrid.sumSurroundingPoints(convertedCoordinates)

    // Change the current coordinates to be the sum
    replace(convertedCoordinates, surroundingSum)
  }

  def checkForAppend(coordinates: Coordinates): Grid = {
    if( coordinates.x + 1 >= size - 1 ||
        coordinates.x - 1 <= 0        ||
        coordinates.y + 1 >= size - 1 ||
        coordinates.y - 1 <= 0) {
      appendZeroBorder()
    } else {
      this
    }
  }

  def appendZeroBorder(): Grid = {
    val gridListsExtendedRows = grid.map(row => 0 +: row :+ 0)
    val newGrid = List.fill(gridListsExtendedRows(0).length)(0) +: gridListsExtendedRows :+ List.fill(gridListsExtendedRows(0).length)(0)
    Grid(newGrid)
  }

  def sumSurroundingPoints(coordinates: Coordinates): Int = {

    val sum = (for {
      i <- coordinates.x - 1 to coordinates.x + 1
      j <- coordinates.y - 1 to coordinates.y + 1
      if !(i == coordinates.x && j == coordinates.y)
      d = grid(j)(i)
    } yield d).sum

    sum
  }

  def replace(coordinates: Coordinates, newValue: Int): Grid = {
    Grid(grid.updated(coordinates.y, grid(coordinates.y).updated(coordinates.x, newValue)))
  }

  def size: Int = {
    grid.size
  }

  def atCorner(coordinates: Coordinates): Boolean = {
      // Top right corner
      (coordinates.x == grid.size -1 && coordinates.y == 0) ||
      // Top left corner
      (coordinates.x == 0 && coordinates.y == 0) ||
      // Bottom left corner
      (coordinates.x == 0 && coordinates.y == grid.size - 1) ||
      // Bottom right corner
      (coordinates.x == grid.size - 1 && coordinates.y == grid.size - 1)
  }

}

object Mover {

  def move(position: Position): Position = {

    val nextGrid = position.grid.calculateNextGrid(position.coordinates)

    val nextX = position.coordinates.x + position.direction.increment.x
    val nextY = position.coordinates.y + position.direction.increment.y
    val nextCoordinates = Coordinates(nextX, nextY)

    // If we are at a corner then change direction.
//    if(nextGrid.atCorner(
//    ))
    val nextDirection = position.direction.nextDirection

    Position(nextGrid, nextDirection, nextCoordinates)

  }

}

object Direction extends Enumeration {

  protected class Val(nextDirectionName: => Direction, val increment: Coordinates) extends super.Val {
    lazy val nextDirection = nextDirectionName
  }

  type Direction = Val

  val NORTH: Val = new Val(Direction.WEST, Coordinates(0, 1))
  val WEST: Val = new Val(Direction.SOUTH, Coordinates(-1, 0))
  val SOUTH: Val = new Val(Direction.EAST, Coordinates(0, -1))
  val EAST: Val = new Val(Direction.NORTH, Coordinates(1, 0))

}
