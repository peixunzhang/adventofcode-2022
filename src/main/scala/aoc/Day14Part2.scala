package aoc
import better.files.Resource
import scala.util.parsing.combinator.RegexParsers

object Day14Part2 extends App {
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

  def fallSand(field: Field, bedrockRow: Int, sandRow: Int, sandCol: Int): Option[Field] = {
    def go(sandRow: Int, sandCol: Int): Option[Field] = {
      // (row, col) is free
      // there is rock below (row, col)
      val bottomFree = isFree(field, bedrockRow, sandRow+1, sandCol)
      lazy val bottomLeftFree = isFree(field, bedrockRow, sandRow+1, sandCol-1)
      lazy val bottomRightFree = isFree(field, bedrockRow, sandRow+1, sandCol+1)
      if (bottomFree) { // down
        go(sandRow+1, sandCol)
      } else if (bottomLeftFree) { // left
        go(sandRow+1, sandCol-1)
      } else if (bottomRightFree) { // right
        go(sandRow+1, sandCol+1)
      } else if (!bottomLeftFree && !bottomRightFree) { // here
        Some(setLocation(field, sandRow, sandCol, Sand))
      } else None
    }
    if (isFree(field, bedrockRow, sandRow, sandCol)) go(sandRow, sandCol) else None
  }

  def getLocation(field: Field, row: Int, col: Int): Option[Stuff] = field.get((row, col))

  def isFree(field: Field, bedrockRow: Int, row: Int, col: Int): Boolean = !field.contains((row, col)) && row < bedrockRow

  def setLocation(field: Field, row: Int, col: Int, stuff: Stuff): Field = field.updated((row, col), stuff)
  
  def convergeSand(field: Field, row: Int, col: Int): Field = {
    val bedrockRow = field.keys.map(_._1).max + 2
    def go(field: Field): Field =
      fallSand(field, bedrockRow, row, col) match {
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
