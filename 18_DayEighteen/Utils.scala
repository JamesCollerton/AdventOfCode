package Utils

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import scala.language.reflectiveCalls

import Instruction._

object Utils {

	val FILE_LOCATION = "Input/"

	def readIn(fileName: String): ArrayBuffer[Instruction] = {

		def readRows(resource: Source): ArrayBuffer[Instruction] = {
			val rows = ArrayBuffer[Instruction]()

			for (line <- resource.getLines) {
				val instruction = new Instruction(line)
				//rows += line.split(" ").map(_.trim)
				rows += instruction
			}

			printRows(rows)

			rows
		}

		def printRows(rows : ArrayBuffer[Instruction]): Unit = {
			rows.foreach(_.printline())
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

