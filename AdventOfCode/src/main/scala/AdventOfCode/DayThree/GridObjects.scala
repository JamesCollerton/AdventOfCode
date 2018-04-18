package AdventOfCode.DayThree

import AdventOfCode.DayThree.Direction.Direction

case class Coordinates(x: Int, y: Int) {

  def convertXCoordinate(grid: Grid): Int = {
    x
  }

  def convertYCoordinate(grid: Grid): Int = {
    y
  }

  def convert(grid: Grid): Coordinates = {
    Coordinates(x, y)
  }

}

case class Position(grid: Grid, direction: Direction, coordinates: Coordinates)

case class Grid(grid: List[List[Int]]) {

  def calculateNextGrid(coordinates: Coordinates): Grid = {
    // If we are going to go off the end need to add an extra row. Note,
    // we always want the grid coordinates to be centred on zero.
    val convertedCoordinates = coordinates.convert(this)

    // Otherwise sum all of the surrounding areas
    Grid(grid)
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

}

object Mover {

  def move(position: Position): Position = {

    val nextGrid = position.grid.calculateNextGrid(position.coordinates)

    val nextX = position.coordinates.x + position.direction.increment.x
    val nextY = position.coordinates.y + position.direction.increment.y
    val nextCoordinates = Coordinates(nextX, nextY)

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
