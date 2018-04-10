package AdventOfCode

import java.io.InputStream

import scala.io.Source

object Utils {

  def readFileAsList(filename: String): List[String] = {
     val stream: InputStream = getClass.getResourceAsStream(filename)
     Source.fromInputStream( stream ).getLines.toList
  }

}
