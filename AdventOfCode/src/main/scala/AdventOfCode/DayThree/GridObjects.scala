package AdventOfCode.DayThree

import AdventOfCode.DayThree.Direction.Direction

case class Position(grid: Grid, direction: Direction, coordinates: Coordinates)

case class Solution(solved: Boolean, answer: Int)

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
    val newGrid = checkForAppend(convertedCoordinates)

    // Convert the coordinates from (x, y) to array coordinates.
    val appendedConvertedCoordinates = coordinates.convert(newGrid)

    // Sum all of the surrounding areas
    val surroundingSum = newGrid.sumSurroundingPoints(appendedConvertedCoordinates)

    if(surroundingSum > 325489) {
      println("Answer " + surroundingSum)
      System.exit(0)
    }

    // Change the current coordinates to be the sum
    replace(convertedCoordinates, surroundingSum)
  }

  def checkForAppend(coordinates: Coordinates): Grid = {
    if( coordinates.x >= size - 1 ||
        coordinates.x <= 0        ||
        coordinates.y >= size - 1 ||
        coordinates.y <= 0) {
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

}

object Mover {

  def move: Unit = {

    // Terribly coded, but just exits in the program
    def loop(position: Position, sideLength: Int): Unit = {
      val newPosition = makeCircle(position, sideLength)
      loop(newPosition, sideLength + 2)
    }

    val startGrid = Grid(List(List((1)))).appendZeroBorder()
    val startCoordinates = Coordinates(1, 0)
    val startDirection = Direction.NORTH
    val startPosition = Position(startGrid, startDirection, startCoordinates)

    loop(startPosition, 1)

  }

  def makeCircle(position: Position, sideLength: Int): Position = {

    // Move up
    val topRight = moveSide(position, sideLength)
    // Move left
    val topLeft = moveSide(topRight, sideLength + 1)
    // Move down
    val bottomLeft = moveSide(topLeft, sideLength + 1)

    val expandedBottomLeft = Position(bottomLeft.grid.appendZeroBorder(), bottomLeft.direction, bottomLeft.coordinates)

    // Move right again
    moveSide(expandedBottomLeft, sideLength + 2)

  }

  @annotation.tailrec
  def moveSide(position: Position, remainingSteps: Int): Position = {

    // Calculate new grid
    val nextGrid = position.grid.calculateNextGrid(position.coordinates)

    // If we're done then turn on the spot
    if(remainingSteps == 0) {
      return Position(nextGrid, position.direction.nextDirection, position.coordinates)
    }

    // Otherwise move forward
    val nextX = position.coordinates.x + position.direction.increment.x
    val nextY = position.coordinates.y + position.direction.increment.y
    val nextCoordinates = Coordinates(nextX, nextY)

    moveSide(Position(nextGrid, position.direction, nextCoordinates), remainingSteps - 1)

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
