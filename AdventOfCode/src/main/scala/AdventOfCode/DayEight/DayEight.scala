package AdventOfCode.DayEight

import AdventOfCode.Utils.ReadFileUtils

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object DayEight {

  def main(args: Array[String]): Unit = {
    val registersMax = part("/AdventOfCode/DayEight/DayEight.txt")
    println(partOne(registersMax._1))
    println(registersMax._2)
  }

  def part(filename: String): (Map[String, Int], Int) = {
    val instructions = parseInput(filename)
    val registers = createRegisters(instructions)
    findAllRegisters(instructions, registers, 0)
  }

  def partOne(registers: Map[String, Int]): Int = {
    registers.maxBy(_._2)._2
  }

  def parseInput(filename: String): List[Instruction] = {
     ReadFileUtils.readFileAsStringList(filename).map(parseLine)
  }

  def parseLine(line: String): Instruction = {
    val splitLine = line.split("\\s+")
    Instruction(splitLine(0), splitLine(1), splitLine(2).toInt, splitLine(4), splitLine(5), splitLine(6).toInt)
  }

  def createRegisters(instructions: List[Instruction]): Map[String, Int] = {
    instructions.map(i => i.regToChange).distinct.map(reg => (reg, 0)).toMap
  }

  @annotation.tailrec
  def findAllRegisters(instructions: List[Instruction], registers: Map[String, Int], max: Int): (Map[String, Int], Int) = {
    if(instructions.isEmpty) {
      return (registers, max)
    }

    val currentInstruction = instructions.head
    val shouldUpdate = currentInstruction.shouldUpdate(registers)

    val (updatedRegisters, newMax) = if(shouldUpdate) {

      val newValue = currentInstruction.getNewValue(registers)

      val newMax = if(newValue > max) newValue else max

      (registers + (currentInstruction.regToChange -> newValue), newMax)

    } else {

      (registers, max)

    }

    findAllRegisters(instructions.tail, updatedRegisters, newMax)

  }

  def runTimeExecute(code: String): Boolean = {
    val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
    tb.eval(tb.parse(code)).asInstanceOf[Boolean]
  }

  case class Instruction(regToChange: String, action: String, changeAmount: Int, regToCheck: String, operation: String, checkAmount: Int) {

    def shouldUpdate(registers: Map[String, Int]): Boolean = {
      val valueToCompare = registers(regToCheck)
      runTimeExecute(valueToCompare + " " + operation + " " + checkAmount)
    }

    def getNewValue(registers: Map[String, Int]): Int = {
      if(action == "inc") {
        registers(regToChange) + changeAmount
      } else {
        registers(regToChange) - changeAmount
      }
    }

  }

}
