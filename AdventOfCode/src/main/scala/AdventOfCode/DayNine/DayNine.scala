package AdventOfCode.DayNine

import AdventOfCode.Utils.ReadFileUtils

object DayNine {

  /**
    * Groups are represented with {}
    * Garbage is represented by <> (ignore characters until the close of garbage)
    * ! is used to cancel characters (ignore next character)
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    val input = ReadFileUtils.readFileAsString("/AdventOfCode/DayNine/DayNine.txt").split("")
    println(countGroups(input, false, 0, 0))
    println(countGarbage(input, false, 0))
  }

  @annotation.tailrec
  def countGroups(characters: Array[String], inGarbage: Boolean, numEnclosedGroups: Int, runningScore: Int): Int = {

    if(characters.length == 0) return runningScore

    val (newCharacters, newInGarbage, newNumEnclosedGroups, newRunningScore) = (characters.head, inGarbage) match {
      case("{", false) => (characters.tail, inGarbage, numEnclosedGroups + 1, runningScore)
      case ("}", false) => (characters.tail, inGarbage, numEnclosedGroups - 1, runningScore + numEnclosedGroups)
      case ("<", _) => (characters.tail, true, numEnclosedGroups, runningScore)
      case (">", _) => (characters.tail, false, numEnclosedGroups, runningScore)
      case ("!", _) => (characters.tail.tail, inGarbage, numEnclosedGroups, runningScore)
      case _ => (characters.tail, inGarbage, numEnclosedGroups, runningScore)
    }

    countGroups(newCharacters, newInGarbage, newNumEnclosedGroups, newRunningScore)

  }

  @annotation.tailrec
  def countGarbage(characters: Array[String], inGarbage: Boolean, runningCount: Int): Int = {

    if(characters.length == 0) return runningCount

    val (newCharacters, newIsGarbage, newRunningCount) = (characters.head, inGarbage) match {
      case ("<", true) => (characters.tail, true, runningCount + 1)
      case ("<", false) => (characters.tail, true, runningCount)
      case (">", _) => (characters.tail, false, runningCount)
      case ("!", _) => (characters.tail.tail, inGarbage, runningCount)
      case (_, true) => (characters.tail, inGarbage, runningCount + 1)
      case _ => (characters.tail, inGarbage, runningCount)
    }

    countGarbage(newCharacters, newIsGarbage, newRunningCount)
  }

}
