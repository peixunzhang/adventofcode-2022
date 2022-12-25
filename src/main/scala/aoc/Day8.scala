package aoc
import better.files.Resource

object Day8 extends App {
  val data = Resource.getAsString("day8/real.txt")
  val forest = data.split("\n").toVector.map(parseRow)

  val numberOfRows = forest.size
  val numberOfCols = forest(0).size

  def parseRow(data: String): Vector[Int] = {
    data.split("").map(_.toInt).toVector
  }

  def getHeight(row: Int, col: Int): Int =
    forest(row)(col)

  def isVisible(row: Int, col: Int): Boolean = {
    val ownHeight = getHeight(row, col)

    (0 until row).forall(getHeight(_, col) < ownHeight) || // left
    (0 until col).forall(getHeight(row, _) < ownHeight) || // front
    (row + 1 until numberOfRows).forall(getHeight(_, col) < ownHeight) || // right
    (col + 1 until numberOfCols).forall(getHeight(row, _) < ownHeight) // back
  }

  val numberOfVisibleTrees = {
    for {
      row <- 0 until numberOfRows
      col <- 0 until numberOfCols
      if isVisible(col, row)
    } yield ()
  }.size

  def countWhile[A](data: Iterable[A])(f: A => Boolean): Int = {
    def go(data: List[A], acc: Int): Int = data match {
      case Nil => acc
      case x :: xs => if (f(x)) go(xs, acc + 1) else acc + 1
    }
    go(data.toList, 0)
  }

  def scenicScore(row: Int, col: Int): Int = {
    val ownHeight = getHeight(row, col)

    countWhile(row - 1 to 0 by -1)(getHeight(_, col) < ownHeight) *
    countWhile(col - 1 to 0 by -1)(getHeight(row, _) < ownHeight) *
    countWhile(row + 1 until numberOfRows)(getHeight(_, col) < ownHeight) *
    countWhile(col + 1 until numberOfCols)(getHeight(row, _) < ownHeight)
  }

  val bestScenicScore = {
    for {
      row <- 1 until numberOfRows - 1
      col <- 1 until numberOfCols - 1
      // if !isVisible(row, col)
    } yield scenicScore(row, col)
  }.max

  println(bestScenicScore)
}
