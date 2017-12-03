object DayThree {

	// Each number in the bottom right hand side of the
	// square is a square number. We can use this to calculate
	// the coordinates of the input (325489) and then just
	// calculate the manhattan distance from there.

	// Examples:
	// 12: (4, 3) to (2, 2) on a 5 x 5 grid, answer: 3
	// 23: (2, 0) to (2, 2) on a 5 x 5 grid, answer: 2
	// 1024: (1, 32) to (16, 16) on a 33 x 33 grid, answer: 31
	// 325489: (16, 0) to (284, 284) on a 571 x 571 grid, answer: 31

	def main(args: Array[String]): Unit = {
		println("12: " + SolveGrid.calculateDistance((4, 3), (2, 2)))
		println("23: " + SolveGrid.calculateDistance((2, 0), (2, 2)))
		println("1024: " + SolveGrid.calculateDistance((1, 32), (16, 16)))
		println("325489: " + SolveGrid.calculateDistance((16, 0), (284, 284)))
	}		

}
