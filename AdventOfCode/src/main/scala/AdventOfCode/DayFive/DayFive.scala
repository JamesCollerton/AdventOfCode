package AdventOfCode.DayFive

import AdventOfCode.Utils.ReadFileUtils

object DayFive {

  def main(args: Array[String]): Unit = {
    val input = ReadFileUtils.readFileAsIntList("/AdventOfCode/DayFive/DayFive.txt")
    println(partOne(input))
    println(partTwo(input))
  }

  def partOne(input: List[Int]): Int = {
    makeJump(Position(input, 0, 0))(i => i + 1)
  }

  def partTwo(input: List[Int]): Int = {
    makeJump(Position(input, 0, 0))(i => if(i >= 3) i - 1 else i + 1)
  }

  case class Position(instructions: List[Int], currPos: Int, numSteps: Int)

  @annotation.tailrec
  def makeJump(position: Position)(f: Int => Int): Int = {

    if(position.currPos >= position.instructions.size || position.currPos < 0) {
      return position.numSteps
    }

    val newCurrentPosition = position.currPos + position.instructions(position.currPos)

    val currOffset = position.instructions(position.currPos)

    val newOffset = f(currOffset)

    val newInstructions = position.instructions.updated(position.currPos, newOffset)
    val newNumSteps = position.numSteps + 1

    val newPosition = Position(newInstructions, newCurrentPosition, newNumSteps)
    makeJump(newPosition)(f)
  }

}
