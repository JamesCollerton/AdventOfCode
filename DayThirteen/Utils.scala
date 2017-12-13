package Utils

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import scala.language.reflectiveCalls

object Utils {

	val FILE_LOCATION = "Input/"

	def readIn(fileName: String): HashMap[Int, (Int, Int)] = {

		def readRows(resource: Source): HashMap[Int, (Int, Int)] = {

			val rows = new HashMap[Int, (Int, Int)]	

			for (line <- resource.getLines) {
				rows(line.split(":")(0).trim.toInt) = (line.split(":")(1).trim.toInt, 0)
			}

			printRows(rows)

			rows
		}

		def printRows(rows: HashMap[Int, (Int, Int)]): Unit = {
			rows.foreach{ case (key, value) => println("Key " + key + " Values " + value._1 + " " + value._2) }
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

