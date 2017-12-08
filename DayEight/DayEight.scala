import Utils._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object DayEight {

	def main(args: Array[String]): Unit = {
		val input = Utils.readIn("DayEightInput.txt")	
		execute(input)
	}

	// Go through list and
	// 	- Get register name
	// 	- If register in HashMap then amend value, if not add
	// 	- Execute the if statement
	def execute(regInstructions: ArrayBuffer[Array[String]]) { 
		
		val registerResults: HashMap[String, Int] = new HashMap() 

		regInstructions.foreach(
			row => {
				// If register we're modifying doesn't exist
				if(!registerResults.contains(row(0))) registerResults(row(0)) = 0
				
				// If register in if doesn't exist
				val ifRegister = row(3).split(" ")(0)
				if(!registerResults.contains(ifRegister)) registerResults(ifRegister) = 0
				
				// Add value to string from register values
				val executeCode = registerResults(ifRegister).toString + " " + row(3).split(" ").slice(1, row(3).length).mkString(" ")
				println("Code " + executeCode)

				if(runTimeExecute(executeCode)) {
					if(!registerResults.contains(row(0))) registerResults(row(0)) = 0
					if(row(1) == "inc") registerResults(row(0)) += row(2).toInt
					else registerResults(row(0)) -= row(2).toInt
				}
			}
		)
	
		registerResults.foreach(x => println("Reg, value " + x._1 + ", " + x._2))
		println("Max value " + registerResults.map(x => x._2).max)
	
	}

	def runTimeExecute(code: String): Boolean = {
		println(code)
		val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
		tb.eval(tb.parse(code)).asInstanceOf[Boolean]
	}

}
