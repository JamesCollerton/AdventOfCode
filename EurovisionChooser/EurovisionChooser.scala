import scala.util.Random

object EurovisionChooser {

	def main(args: Array[String]): Unit = {

		val countries = List(
					"Austria",
					"Estonia",
					"Cyprus",
					"Lithuania",
					"Israel",
					"Czech",
					"Bulgaria",
					"Albania",
					"Finland",
					"Ireland",
					"Serbia",
					"Moldova",
					"Hungary",
					"Ukraine",
					"Sweden",
					"Australia",
					"Norway",
					"Denmark",
					"Slovenia",
					"The",
					"UK",
					"France",
					"Germany",
					"Italy",
					"Spain",
					"Portugal"
		)

		val shuffled = Random.shuffle(countries)
		val groups = shuffled.grouped(shuffled.length / 5)
		val names = List("Ali", "Ruth", "Freddie", "James", "Yumi")	
	
		groups.foreach(println)
	
	}

}
