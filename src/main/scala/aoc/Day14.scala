package aoc
import better.files.Resource
import scala.util.parsing.combinator.RegexParsers

object Day14 extends App {
  type Line = List[(Int, Int)]
  type Field = Map[(Int, Int), Stuff]
  
  sealed trait Stuff
  case object Rock extends Stuff
  case object Sand extends Stuff

  def parseLine(line: String): Line =
    line.split(" -> ").map(_.split(",").toList).collect { case col :: row :: Nil => (row.toInt, col.toInt) }.toList

  def drawLine(field: Field, line: Line): Field = {
    def go(field: Field, line: Line, previous: Option[(Int, Int)]): Field = 
      line match {
        case Nil => field
        case (nextPoint @ (nextRow, nextCol)) :: xs =>
          val updatedField: Field = previous.fold(field) { case (previousRow, previousCol) =>
            if (previousRow == nextRow) { // vertical line
              val points = Math.min(previousCol, nextCol) to Math.max(previousCol, nextCol)
              points.foldLeft(field) { case (acc, col) =>  acc.updated((previousRow, col), Rock) }
            } else if (previousCol == nextCol) {
              val points = Math.min(previousRow, nextRow) to Math.max(previousRow, nextRow)
              points.foldLeft(field) { case (acc, row) =>  acc.updated((row, previousCol), Rock) }
            } else {
              throw new IllegalArgumentException("invalid line")
            }
          }
          go(updatedField, xs, Some(nextPoint))
      }
    go(field, line, None)
  }

  def makeField(lines: List[Line]): Field = lines.foldLeft(Map.empty: Field)(drawLine)

  def fallSand(field: Field, lowest: Map[Int, Int], sandRow: Int, sandCol: Int): Option[Field] = {
    def go(sandRow: Int, sandCol: Int): Option[Field] = {
      // (row, col) is free
      // there is rock below (row, col)
      val bottomFree = isFree(field, sandRow+1, sandCol)
      lazy val bottomLeftFree = isFree(field, sandRow+1, sandCol-1)
      lazy val bottomRightFree = isFree(field, sandRow+1, sandCol+1)
      if (bottomFree) { // down
        go(sandRow+1, sandCol)
      } else if (bottomLeftFree && hasRockBelow(lowest, sandRow+1, sandCol-1)) { // left
        go(sandRow+1, sandCol-1)
      } else if (bottomRightFree && hasRockBelow(lowest, sandRow+1, sandCol+1)) { // right
        go(sandRow+1, sandCol+1)
      } else if (!bottomLeftFree && !bottomRightFree) { // here
        Some(setLocation(field, sandRow, sandCol, Sand))
      } else None
    }
    if (isFree(field, sandRow, sandCol) && hasRockBelow(lowest, sandRow, sandCol)) go(sandRow, sandCol) else None
  }

  def getLocation(field: Field, row: Int, col: Int): Option[Stuff] = field.get((row, col))

  def isFree(field: Field, row: Int, col: Int): Boolean = !field.contains((row, col))

  def setLocation(field: Field, row: Int, col: Int, stuff: Stuff): Field = field.updated((row, col), stuff)

  def hasRockBelow(lowest: Map[Int, Int],row: Int, col: Int ): Boolean = lowest.get(col).fold(false)(_ > row )
  
  def convergeSand(field: Field, row: Int, col: Int): Field = {
    val lowest: Map[Int, Int] = field.foldLeft(Map.empty[Int, Int]) { 
      case (acc, ((row, col), Rock)) =>
        val newMin = acc.get(col).fold(row)(Math.max(_, row))
        acc.updated(col, newMin)
      case (acc, (_, Sand)) => acc
    }
    def go(field: Field): Field =
      fallSand(field, lowest, row, col) match {
        case None => field
        case Some(next) => go(next)
      }

    go(field)
  }

  // run it
  val data = Resource.getAsString("day14/real.txt").split("\n").toList
  val field: Field = makeField(data.map(parseLine))
  val sandBlocks = convergeSand(field, 0, 500).values.count(_ == Sand)
  println(sandBlocks)
}
