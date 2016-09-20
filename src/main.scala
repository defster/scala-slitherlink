import java.io.File;

object main {

  def main(args: Array[String]): Unit = {
    val inputDir = "Input";
    val outputDir = "Output";
    val puzzles = new Puzzles(inputDir);

    printPuzzle(puzzles.getPuzzle(3));
    new solver().solvePuzzle(puzzles.getPuzzle(3));

//    val u = -1;
//    val r = -2;
//    val d = -4;
//    val l = -8;
//
//    val testsol = Array(
//      Array(r, u, r, u, r, u),
//      Array(r, l, u, l, r, l),
//      Array(d, d, r, l, r, l),
//      Array(r, u, u, l, u, l),
//      Array(r, d, r, d, r, l),
//      Array(d, l, d, l, d, l));
//
//    val vector:Vector[Vector[Int]] = Vector.tabulate(6)((y) => testsol(y).toVector);
//    val solution = new Solution(vector, 0, 0);
//    printSolution(solution)
//
//    val smalltest = Array(
//      Array(r, u, u),
//      Array(r, 0, l),
//      Array(d, d, l));
//
//    val vector:Vector[Vector[Int]] = Vector.tabulate(3)((y) => smalltest(y).toVector);
//    val solution = new Solution(vector, 0, 0);
//    printSolution(solution)
  }

  def printPuzzle(puzzle:Array[Array[Int]]) = {
    val xSize = puzzle(0).length;
    val ySize = puzzle.length;

    for (y <- Range(0, ySize)){
      for (x <- Range(0, xSize)){
        val square = puzzle(x)(y);
        if (square == -2) print("* ");
        else print(square + " ");
      }
      print('\n');
    }
    print("\n");
  }

}
