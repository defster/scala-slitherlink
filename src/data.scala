

class Solution(grd:Vector[Vector[Int]], xpn:Int, ypn:Int) {
  def this(xSize:Int, ySize:Int, xStart:Int, yStart:Int) = {
    this(Vector.fill(xSize)(Vector.fill(ySize)(15)), xStart, yStart);
  }

  val grid = grd;
  val xp = xpn;
  val yp = ypn;

  def getVertex(x:Int, y:Int):Int = grid(x)(y);
  def getGrid():Vector[Vector[Int]] = grid;
  def xPrev():Int = xp;
  def yPrev():Int = yp;

  def isVisited(x:Int, y:Int):Boolean = return grid(x)(y) < 0;

  def addPoint(x:Int, y:Int):Solution = {
    if (x > xp) return new Solution(grid.updated(x, grid(x).updated(y, -8)), x, y);
    if (x < xp) return new Solution(grid.updated(x, grid(x).updated(y, -2)), x, y);
    if (y > yp) return new Solution(grid.updated(x, grid(x).updated(y, -1)), x, y);
    if (y < yp) return new Solution(grid.updated(x, grid(x).updated(y, -4)), x, y);
    println("something is wrong!!!");
    return this;
  }
}