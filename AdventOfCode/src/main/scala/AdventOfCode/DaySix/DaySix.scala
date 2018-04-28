package AdventOfCode.DaySix

import AdventOfCode.Utils.ReadFileUtils

object DaySix {

  def main(args: Array[String]): Unit = {
    val input = ReadFileUtils.readFileAsIntListList("/AdventOfCode/DaySix/DaySix.txt")(0)
    println(partOne(input))
    println(partTwo(input))
  }

  def partOne(input: List[Int]): Int = {
    val (stoppingConfig, steps) = nextStep(input, List(input), 0, List())((conf, newInp, inp) => conf.contains(newInp))
    steps
  }

  def partTwo(input: List[Int]): Int = {
    val (stoppingConfig, steps) = nextStep(input, List(input), 0, List())((conf, newInp, inp) => conf.contains(newInp))
    val (secondStoppingConfig, secondSteps) = nextStep(stoppingConfig, List(input), 0, stoppingConfig)((conf, newInp, inp) => newInp == inp)
    secondSteps
  }

  @annotation.tailrec
  def nextStep(input: List[Int], configurations: List[List[Int]], steps: Int, staticInput: List[Int])(f: (List[List[Int]], List[Int], List[Int]) => Boolean): (List[Int], Int) = {

    val maxIndex = findMaxIndex(input)
    val maxValue = input(maxIndex)
    val resetMaxBlock = input.updated(maxIndex, 0)

    val newInput = redistributeBlocks(resetMaxBlock, (maxIndex + 1) % input.length, maxValue)

    if(f(configurations, newInput, staticInput)) return (input, steps + 1)

    val newConfigurations = configurations :+ newInput

    nextStep(newInput, newConfigurations, steps + 1, staticInput)(f)
  }

  @annotation.tailrec
  def redistributeBlocks(input: List[Int], currentIndex: Int, remainingBlocks: Int): List[Int] = {
    if(remainingBlocks == 0) return input

    val newInput = input.updated(currentIndex, input(currentIndex) + 1)

    redistributeBlocks(newInput, (currentIndex + 1) % newInput.length, remainingBlocks - 1)
  }

  def findMaxIndex(input: List[Int]): Int = {
    input.zipWithIndex.maxBy(_._1)._2
  }

}
