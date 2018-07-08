package AdventOfCode.DayNine

object DayNine {

  /**
    * Groups are represented with {}
    * Garbage is represented by <> (ignore characters until the close of garbage)
    * ! is used to cancel characters (ignore next character)
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    print("Hello, world")
  }

  def countGroups(characters: Array[String], inGarbage: Boolean, numEnclosedGroups: Int): Int = {

    if(characters.length == 0) return 0

    val numberGroups = (characters.head, inGarbage) match {
      case("{", false) => countGroups(characters.tail, inGarbage, numEnclosedGroups + 1)
      case ("}", false) => countGroups(characters.tail, inGarbage, numEnclosedGroups - 1) + numEnclosedGroups
      case ("<", _) => countGroups(characters.tail, true, numEnclosedGroups)
      case (">", _) => countGroups(characters.tail, false, numEnclosedGroups)
      case ("!", _) => countGroups(characters.tail.tail, inGarbage, numEnclosedGroups)
      case _ => countGroups(characters.tail, inGarbage, numEnclosedGroups)
    }

    numberGroups
  }

}
