package AdventOfCode

import scala.io.Source

object Utils {

  def readFile(filename: String): List[String] = {
     Source.fromFile(filename).getLines.toList
  }

}
