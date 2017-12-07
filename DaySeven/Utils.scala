package Utils

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import scala.language.reflectiveCalls

object Utils {

	val FILE_LOCATION = "Input/"

	// Read in every row
	// All before -> Is the name of the program
	// All after -> Separated by comma is supported programs

	def readIn(fileName: String): HashMap[String, Array[String]] = {

		def readRows(resource: Source): HashMap[String, Array[String]] = {
			val programMap: HashMap[String, Array[String]] = HashMap()
	
			for (line <- resource.getLines) {
				val programInfo = line.split("->").map(_.trim)
				val supportedPrograms: Array[String] = if(programInfo.length > 1) {
					programInfo(1).split(",").map(_.trim);
				} else {
					Array[String]()
				}
				programMap(programInfo(0)) = supportedPrograms
			}

			printRows(programMap)

			programMap
		}

		def printRows(programMap : HashMap[String, Array[String]]): Unit = {
			programMap.foreach(prog => println(prog._1 + " -> " + prog._2.mkString(" ")))
		}

		def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B = {
			try {
				f(resource)
			} finally {
				resource.close;
			}
		}

		using(Source.fromFile(FILE_LOCATION + fileName))(readRows);		

	}

}

