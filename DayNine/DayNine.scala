import Utils._

import scala.collection.immutable.List

object DayNine {

	def main(args: Array[String]): Unit = {
		
		// Testing One
		//val fileNumberList = List("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight")
		//fileNumberList.foreach(num => runOne("DayNineTestInput" + num + ".txt"));

		// Solution Two (now returns garbage score)
		getScore("DayNineInput.txt")
	}

	def getScore(inputFile: String): Unit = {
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
			if(skip) { skip = false }
			else if(!garbage) {
				if(ch == '<') 		{ garbage = true } 
				else if(ch == '{') 	{ bracketStack ::= "{" } 
				else if(ch == '}') 	{
					score += bracketStack.length
					bracketStack = bracketStack.tail
				}
			} else if(garbage) {
				if(ch == '!') 		{ skip = true } 
				else if(ch == '>') 	{ garbage = false } 
				else { garbageScore += 1 }
			} 
		})

		score
	}

}
