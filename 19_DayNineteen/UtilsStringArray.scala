package Utils

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import scala.language.reflectiveCalls

object Utils {

	val FILE_LOCATION = "Input/"

	def readIn(fileName: String): ArrayBuffer[Array[String]] = {

		def readRows(resource: Source): ArrayBuffer[Array[String]] = {
			val rows = ArrayBuffer[Array[String]]()

			for (line <- resource.getLines) {
				rows += "" +: line.split("").map(_.trim) :+ ""
			}

			val blankRow = Array.fill(rows(0).length)("")
			
			rows += blankRow
			//printRows(rows)

			rows
		}

		def printRows(rows : ArrayBuffer[Array[String]]): Unit = {
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

