import java.io.File;

class Puzzles(pDir:String) {
  private val dir = new File(pDir);
  private val puzzleList = importFiles(dir);

  def getPuzzle(n:Int):Array[Array[Int]] = puzzleList(n);
  def getPuzzles():List[Array[Array[Int]]] = puzzleList;

  private def importFiles(dir:File):List[Array[Array[Int]]] = {
    var list = List[Array[Array[Int]]]();

    for (file <- dir.listFiles()){
      list = list ::: importPuzzles(file);
    }

    return list;
  }

  private def importPuzzles(file:File):List[Array[Array[Int]]] = {

    val lines = scala.io.Source.fromFile(file).mkString.split("\n");
    val numberOfPuzzles:Int = lines(0).toInt;
    var puzzles = List[Array[Array[Int]]]();

    var offset = 1;

    for (i <- Range(0, numberOfPuzzles)){
      val xSize = lines(offset).split('x')(0).toInt;
      val ySize = lines(offset).split('x')(1).toInt;
      offset += 1;
      puzzles = puzzles :+ importPuzzle(lines, offset, xSize, ySize);
      offset += ySize;
    }

    def importPuzzle(lines:Array[String], offset:Int, xSize:Int, ySize:Int):Array[Array[Int]] = {
      var puzzle = Array.ofDim[Int](xSize, ySize);
      for (y <- Range(0, ySize)){
        val row = lines(offset+y).split(' ');
        for (x <- Range(0, xSize)){
          if (row(x) == "*") puzzle(x)(y) = -2;
          else puzzle(x)(y) = row(x).toInt;
        }
      }
      return puzzle;
    }

    return puzzles;
  }

}
