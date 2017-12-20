package Utils

import ThreeDVector._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import scala.language.reflectiveCalls

object Utils {

	val FILE_LOCATION = "Input/"

	def readIn(fileName: String): ArrayBuffer[PropertiesVector] = {

		def readRows(resource: Source): ArrayBuffer[PropertiesVector] = {
			val rows = ArrayBuffer[PropertiesVector]()

			for (line <- resource.getLines) {
				// Split by space:
				val vectors = line.split(" ").map(_.trim)
				val vectorContents = vectors.map(str => str.slice(str.indexOf("<") + 1, str.indexOf(">")).split(","))
				val vectorObjects = vectorContents.map(arr => new ThreeDVector(arr(0).toInt, arr(1).toInt, arr(2).toInt))
				val propertiesVector = new PropertiesVector(vectorObjects(0), vectorObjects(1), vectorObjects(2))
				rows += propertiesVector
			}

			//printRows(rows)

			rows
		}

		def printRows(rows : ArrayBuffer[PropertiesVector]): Unit = {
			rows.foreach(row => {
				println()
				println(row.p.x + "," + row.p.y + "," + row.p.z)
				println(row.v.x + "," + row.v.y + "," + row.v.z)
				println(row.a.x + "," + row.a.y + "," + row.a.z)
			}) 
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

