
class solver() {

  private var counter = 0;

  // TODO: find a start vertex automatically
  private val xStart = 0;
  private val yStart = 0;

  // TODO: output should be in its own class
  private def printSolution(solution:Solution, x1:Int = xStart, y1:Int = yStart):Unit = {
    val grid = solution.getGrid();
    val xSize = grid.length;
    val ySize = grid(0).length;
    var solutionOutput = createBlankSolution(xSize, ySize);

    var current = grid(xStart)(yStart);
    var x = x1;
    var y = y1;

    do{
      val xPrev = x;
      val yPrev = y;
      if (current == -1) y -= 1;
      if (current == -2) x += 1;
      if (current == -4) y += 1;
      if (current == -8) x -= 1;
      if (x >= xSize || x < 0 || y >= ySize || y < 0){
        println("Error! Not a valid solution! code 1");
        return;
      }
      current = grid(x)(y);
      if (current >= 0){
        println("Error! Not a valid solution! code 2");
        return;
      }
      setLine(xPrev, yPrev, x, y);
    }
    while ((x != xStart || y != yStart));

    for (x <- Range(0, xSize*2)){
      for (y <- Range(0, ySize*2)) {
        print(solutionOutput(y)(x));
      }
      print('\n');
    }
    println("end solution");
    def setLine(x1:Int, y1:Int, x2:Int, y2:Int) = {
      if (x1 == x2){
        val y = Math.min(y1, y2) * 2 + 1;
        val x = x1 * 2;
        solutionOutput(x)(y) = '|';
      }
      else{
        val x = Math.min(x1, x2) * 2 + 1;
        val y = y1 * 2;
        solutionOutput(x)(y) = '-';
      }
    }
  }

  private def createBlankSolution(xSize:Int, ySize:Int):Array[Array[Char]] = {
    val xSolSize = xSize*2+1;
    val ySolSize = ySize*2+1;
    var solution = Array.fill(xSolSize)(Array.fill(ySolSize)(' '));

    for (x <- Range(0, xSolSize)) {
      if (x % 2 == 0){
        for (y <- Range(0, ySolSize)){
          if (y % 2 == 0){
            solution(x)(y) = '+';
          }
        }
      }
    }

    return solution;
  }

  def solvePuzzle(puzzle:Array[Array[Int]]) = {

    val ySize = puzzle.length;
    val xSize = puzzle(0).length;
    val temp = createBlankGrid(xSize, ySize);
    val vector:Vector[Vector[Int]] = Vector.tabulate(ySize+1)((y) => temp(y).toVector);
    val solution = new Solution(vector, xStart, yStart);

    val startTime = java.lang.System.currentTimeMillis();
    if (!solveRecurse(puzzle, solution, 1, 0))
      println("No solution found!");
    println((java.lang.System.currentTimeMillis() - startTime).toDouble/1000)
  }

  private def solveRecurse(puzzle:Array[Array[Int]], solution:Solution, vx:Int, vy:Int):Boolean = {
    val xPrev = solution.xPrev();
    val yPrev = solution.yPrev();
    val x = xPrev + vx;
    val y = yPrev + vy;

    counter += 1;
    if (counter % 5000000 == 0){
      println(counter);
    }

    if (solution.isVisited(x, y)) {
      return false;
    }

    if (x == xStart && y == yStart) {
      val newSolution = solution.addPoint(x, y);
      val newPuzzle = puzzle.map(_.clone);
      if (updatePuzzle(x, y, xPrev, yPrev, vx, vy, newPuzzle) == false){
        return false;
      }
      if (checkSolution(newPuzzle)) {
        println("Solution found!");
        printSolution(newSolution);
        return true;
      }
    }

    val newPuzzle = puzzle.map(_.clone);

    if (updatePuzzle(x, y, xPrev, yPrev, vx, vy, newPuzzle) == false){
      return false;
    }

    val newSolution = solution.addPoint(x, y);

    if ((solution.getVertex(x, y) & 1) == 1){
      if (solveRecurse(newPuzzle, newSolution, 0, -1)) return true;
    }
    if ((solution.getVertex(x, y) & 2) == 2){
      if (solveRecurse(newPuzzle, newSolution, 1, 0)) return true;
    }
    if ((solution.getVertex(x, y) & 4) == 4){
      if (solveRecurse(newPuzzle, newSolution, 0, 1)) return true;
    }
    if ((solution.getVertex(x, y) & 8) == 8){
      if (solveRecurse(newPuzzle, newSolution, -1, 0)) return true;
    }

    return false;
  }

  private def updatePuzzle(x:Int, y:Int, xPrev:Int, yPrev:Int, vx:Int, vy:Int, newPuzzle:Array[Array[Int]]):Boolean = {
    val xSize = newPuzzle.length;
    val ySize = newPuzzle(0).length;
    if (vx == 0){
      val xSquare = x;
      val ySquare = Math.min(y, yPrev);
      if (xSquare > 0){
        if (newPuzzle(xSquare-1)(ySquare) == 0){
          return false;
        }
        newPuzzle(xSquare-1)(ySquare) -= 1;
      }
      if (xSquare < xSize){
        if (newPuzzle(xSquare)(ySquare) == 0){
          return false;
        }
        newPuzzle(xSquare)(ySquare) -= 1;
      }
    }
    else{
      val xSquare = Math.min(x, xPrev);
      val ySquare = y;
      if (ySquare > 0){
        if (newPuzzle(xSquare)(ySquare-1) == 0){
          return false;
        }
        newPuzzle(xSquare)(ySquare-1) -= 1;
      }
      if (ySquare < ySize){
        if (newPuzzle(xSquare)(ySquare) == 0){
          return false;
        }
        newPuzzle(xSquare)(ySquare) -= 1;
      }
    }
    return true;
  }

  private def checkSolution(puzzle:Array[Array[Int]]):Boolean = {
    val xSize = puzzle.length;
    val ySize = puzzle(0).length;

    for (y <- Range(0, ySize)){
      for (x <- Range(0, xSize)){
        val square = puzzle(x)(y);
        if (square > -2){
          if (square != 0){
            return false;
          }
        }
      }
    }
    return true;
  }

  private def createBlankGrid(xSize:Int, ySize:Int):Array[Array[Int]] = {
    val grid = Array.fill(xSize+1)(Array.fill(ySize+1)(15));

    for (x <- Range(0, xSize+1)){
      for (y <- Range(0, ySize+1)){
        if (x == 0) grid(x)(y) = strikeLeft(grid(x)(y), x, y);
        if (x == xSize) grid(x)(y) = strikeRight(grid(x)(y), x, y);
        if (y == 0) grid(x)(y) = strikeUp(grid(x)(y), x, y);
        if (y == ySize) grid(x)(y) = strikeDown(grid(x)(y), x, y);
      }
    }

    return grid;
  }

  private def strikeUp(n:Int, x:Int, y:Int):Int = {
    return n & ~(1 << 0);
  }
  private def strikeRight(n:Int, x:Int, y:Int):Int = {
    return n & ~(1 << 1);
  }
  private def strikeDown(n:Int, x:Int, y:Int):Int = {
    return n & ~(1 << 2);
  }
  private def strikeLeft(n:Int, x:Int, y:Int):Int = {
    return n & ~(1 << 3);
  }

  private def setUp(x:Int, y:Int):Int = {
    return 1;
  }
  private def setRight(x:Int, y:Int):Int = {
    return 2;
  }
  private def setDown(x:Int, y:Int):Int = {
    return 4;
  }
  private def setLeft(x:Int, y:Int):Int = {
    return 8;
  }
}
