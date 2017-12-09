import Utils._

import scala.collection.immutable.List

object DayNine {

	def main(args: Array[String]): Unit = {
		
		// Testing
		val fileNumberList = List("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight")
		fileNumberList.foreach(num => runOne("DayNineTestInput" + num + ".txt"));

		// Solution Two (now returns garbage score)
		runOne("DayNineInput.txt")
	}

	def runOne(inputFile: String): Unit = {
		def inputStream = Utils.readIn(inputFile)(0)(0)
		println("Score: " + calculateScore(inputStream))
	}

	def calculateScore(inputStream: String): Int = {
	
		var score = 0;
		var garbageScore = 0;

		var garbage = false;
		var skip = false;

		var bracketStack = List[String]()

		inputStream.foreach(ch => {
			if(skip){
				skip = false;
			} else if(ch == '<' && !garbage) {
				garbage = true
			} else if(ch == '{' && !garbage) {
				bracketStack ::= "{"
			} else if(ch == '!' && garbage) {
				 skip = true
			} else if(ch == '>' && garbage) {
				garbage = false
			} else if(ch == '}' && !garbage) {
				score += bracketStack.length
				bracketStack = bracketStack.tail
			} else if (garbage) {
				garbageScore += 1
			}
		})

		garbageScore
	}

}
