package AdventOfCode.DayThree

case class Grid(grid: List[List[Int]])

case class Coordinates(x: Int, y: Int)

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
