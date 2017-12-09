import Utils._

import scala.collection.immutable.List

object DayNine {

	def main(args: Array[String]): Unit = {
		val fileNumberList = List("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight")

		fileNumberList.foreach(num => { runOne("DayNineTestInput" + num + ".txt") 
//			def inputStream = Utils.readIn("DayNineTestInput" + num + ".txt")(0)(0)
//			println("Score: " + calculateScore(inputStream))
		});

		runOne("DayNineInput.txt")
	}

	def runOne(inputFile: String): Unit = {
		def inputStream = Utils.readIn(inputFile)(0)(0)
		println("Score: " + calculateScore(inputStream))
	}

	def calculateScore(inputStream: String): Int = {
	
		var score = 0;

		var garbage = false;
		var skip = false;

		var bracketStack = List[String]()

		inputStream.foreach(ch => {
			if(skip){
//				println("Skipping")
				skip = false;
			} else if(ch == '<' && !garbage) {
//				println("Started garbage")
				garbage = true
			} else if(ch == '{' && !garbage) {
//				println("Added bracket")
				bracketStack ::= "{"
			} else if(ch == '!' && garbage) {
//				println("Skipping next one")
				 skip = true
			} else if(ch == '>' && garbage) {
//				println("Closing garbage")
				garbage = false
			} else if(ch == '}' && !garbage) {
//				println("Adding one")
				score += bracketStack.length
//				println(bracketStack.mkString(" "))
				bracketStack = bracketStack.tail
			}
		})

		score
	}

}
