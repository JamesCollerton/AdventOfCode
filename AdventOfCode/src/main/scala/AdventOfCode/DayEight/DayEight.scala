package AdventOfCode.DayEight

import AdventOfCode.Utils.ReadFileUtils

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object DayEight {

  def main(args: Array[String]): Unit = {
    partOne()
  }

  def partOne(): Unit = {
    val instructions = parseInput("/AdventOfCode/DayEight/DayEight.txt")
    val registers = createRegisters(instructions)
    val finalRegisters = findMax(instructions, registers)
  }

  def parseInput(filename: String): List[Instruction] = {
     ReadFileUtils.readFileAsStringList(filename).map(parseLine)
  }

  def parseLine(line: String): Instruction = {
    val splitLine = line.split("\\s+")
    Instruction(splitLine(0), splitLine(1), splitLine(2).toInt, splitLine(4), splitLine(5), splitLine(6).toInt)
  }

  def createRegisters(instructions: List[Instruction]): List[Register] = {
    instructions.map(i => i.regToChange).distinct.map(reg => Register(reg, 0))
  }

  def findMax(instructions: List[Instruction], registers: List[Register]): List[Register] = {
    if(instructions.length == 0) {
      return registers;
    }
    val currIns = instructions(0)
    val valueToCompare = registers(currIns.regToCheck)
    val shouldUpdate = runTimeExecute(valueToCompare + " " + currIns.operation + " " + currIns.checkAmount)

    return registers
  }

  def runTimeExecute(code: String): Boolean = {
    val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
    tb.eval(tb.parse(code)).asInstanceOf[Boolean]
  }

  case class Instruction(regToChange: String, action: String, changeAmount: Int, regToCheck: String, operation: String, checkAmount: Int)

  case class Register(name: String, value: Int)

}
