package Utils

import BridgePiece._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import scala.language.reflectiveCalls

object Utils {

	val FILE_LOCATION = "Input/"

	def readIn(fileName: String): ArrayBuffer[BridgePiece] = {

		def readRows(resource: Source): ArrayBuffer[BridgePiece] = {
			val rows = ArrayBuffer[BridgePiece]()

			for (line <- resource.getLines) {
				rows += new BridgePiece(line.split("/").map(_.trim.toInt))
			}

			// printRows(rows)

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

