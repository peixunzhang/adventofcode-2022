package aoc
import better.files.Resource
import scala.collection.immutable.Queue

object Day12 extends App {
  val data = Resource.getAsString("day12/real.txt")
  val map = data.split("\n").toVector.map(_.toVector)

  val numberOfRows = map.size
  val numberOfCols = map(0).size

  type Map = Vector[Vector[Char]]

  def getChar(row: Int, col: Int): Char = map(row)(col)

  def getHeight(row: Int, col: Int): Int = {
    val char = getChar(row, col)
    if (char == 'S') 'a'.toInt
    else if (char == 'E') 'z'.toInt
    else char.toInt
  }
  
  def getNext(row: Int, col: Int): List[(Int, Int)] = {
    val candidates = List(
      (row - 1, col),
      (row, col - 1),
      (row + 1, col),
      (row, col + 1)
    )

    def isValid(otherRow: Int, otherCol: Int): Boolean =
      otherRow >= 0 && 
      otherRow < numberOfRows &&
      otherCol >= 0 &&
      otherCol < numberOfCols &&
      getChar(otherRow, otherCol) != 'S' &&
      getHeight(otherRow, otherCol) - getHeight(row, col) <= 1

    candidates.filter((isValid _).tupled)
  }

  val start = {
    for {
      row <- 0 until numberOfRows
      col <- 0 until numberOfCols
      if getChar(row, col) == 'S'
    } yield (row, col)
  }.head

  def findPathToEnd(row: Int, col: Int): Option[Int] = {

    def go(queue: Queue[(Int, Int, Set[(Int, Int)], Int)]): Option[Int] = {
      queue.headOption match {
        case None => None
        case Some((row, col, visited, steps)) =>
          if (getChar(row, col) == 'E') Some(steps)
          else {
            val neighbours = 
              getNext(row, col)
                .filterNot(visited.contains)
                .map { case (nRow, nCol) =>
                  (nRow, nCol, visited + ((row, col)), steps + 1)  
                }

            val neighbourLocations = neighbours.map(n => (n._1, n._2)).toSet
            
            go(queue.tail.filterNot(x => neighbourLocations.contains((x._1, x._2))).appendedAll(neighbours))
          }  
      }
    }

    go(Queue((row, col, Set.empty, 0)))
  }


  final case class Location(value: Char, col: Int, row: Int, visited: Boolean) {
    def findNext(next: Location): Boolean = {
      val ownHeight = getHeight(row, col)
      getHeight(next.row, next.col) - ownHeight == 1 && next.visited == false
    }
  }


  def nextStep(map: Map): Map = map
  
  println(findPathToEnd(start._1, start._2))
}
