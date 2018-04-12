package AdventOfCode.Utils

import java.io.InputStream

import scala.io.Source

object ReadFileUtils {

  def readFileAsString(filename: String): String = {
    readFileAsStringList(filename).mkString("")
  }

  def readFileAsInt(filename: String): Int = {
    readFileAsString(filename).toInt
  }

  def readFileAsIntList(filename: String): List[List[Int]] = {
    readFileAsStringList(filename).map(s => s.split("\\s+").map(s => s.toInt).toList)
  }

  def readFileAsStringList(filename: String): List[String] = {
     val stream: InputStream = getClass.getResourceAsStream(filename)
     Source.fromInputStream(stream).getLines.toList
  }

}
