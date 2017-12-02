package Utils

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import scala.language.reflectiveCalls

object Utils {

	val FILE_LOCATION = "Input/"

	def readIn(fileName: String): ArrayBuffer[Array[Int]] = {

		def readRows(resource: Source): ArrayBuffer[Array[Int]] = {
			val rows = ArrayBuffer[Array[Int]]()

			for (line <- resource.getLines) {
				rows += line.split("\t").map(_.trim.toInt)
			}

			//printRows(rows)

			rows
		}

		def printRows(rows : ArrayBuffer[Array[Int]]): Unit = {
			for(fieldRow <- rows){
				println(fieldRow.mkString(" "));
			}
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


