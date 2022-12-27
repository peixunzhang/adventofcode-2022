package aoc
import better.files.Resource
import scala.annotation.nowarn

object Day9 extends App {
  val data = Resource.getAsString("day9/real.txt").split("\n").toList

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Right extends Direction
  case object Left extends Direction

  final case class Snake(headX: Int, headY: Int, tailX: Int, tailY: Int) {

    def move(direction: Direction): Snake = direction match {
      case Up     => copy(headY = headY+1).moveTail
      case Down   => copy(headY = headY-1).moveTail
      case Right  => copy(headX = headX+1).moveTail
      case Left   => copy(headX = headX-1).moveTail
    }

    private def moveTail: Snake = {
      val deltaX = headX - tailX
      val deltaY = headY - tailY
      val total = deltaX.abs + deltaY.abs

      def contract(int: Int): Int =
        if (int < 0) int + 1
        else if (int > 0) int - 1
        else 0

      (deltaX.abs, deltaY.abs) match {
        case (2, 1) => copy(tailX = tailX + contract(deltaX), tailY = tailY + deltaY)
        case (1, 2) => copy(tailX = tailX + deltaX, tailY = tailY + contract(deltaY))
        case (2, 0) => copy(tailX = tailX + contract(deltaX))
        case (0, 2) => copy(tailY = tailY + contract(deltaY))
        case (1, 1) | (1, 0) | (0, 1) | (0, 0) => this
        case _ => throw new IllegalStateException()
      }
    }
  }

  final case class Snake2(x: Int, y: Int, tail: List[(Int, Int)]) {

    def move(direction: Direction): Snake2 = direction match {
      case Up     => copy(y = y+1).moveTail
      case Down   => copy(y = y-1).moveTail
      case Right  => copy(x = x+1).moveTail
      case Left   => copy(x = x-1).moveTail
    }

    private def moveTail: Snake2 = {
      def go(headX: Int, headY: Int, tailToGo: List[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] =
        tailToGo match {
          case Nil => 
            acc.reverse
          case (tailX, tailY) :: xs =>
            val deltaX = headX - tailX
            val deltaY = headY - tailY
            val total = deltaX.abs + deltaY.abs

            val newSegment = (deltaX.abs, deltaY.abs) match {
              case (2, 2) => (tailX + contract(deltaX), tailY + contract(deltaY))
              case (2, 1) => (tailX + contract(deltaX), tailY + deltaY)
              case (1, 2) => (tailX + deltaX, tailY + contract(deltaY))
              case (2, 0) => (tailX + contract(deltaX), tailY)
              case (0, 2) => (tailX, tailY + contract(deltaY))
              case (1, 1) | (1, 0) | (0, 1) | (0, 0) => (tailX, tailY)
              case _ => throw new IllegalStateException()
            }

            go(newSegment._1, newSegment._2, xs, newSegment :: acc)
        }
      copy(tail = go(x, y, tail, Nil))
    }
  }

  @nowarn("msg=exhaustive")
  def applyMoves(snake: Snake, moves: List[Direction]): List[Snake] = 
    moves.foldLeft(List(snake)) { case (snake :: oldSnakes, next) => 
      snake.move(next) :: snake :: oldSnakes
    }.reverse

  @nowarn("msg=exhaustive")
  def applyMoves2(snake: Snake2, moves: List[Direction]): List[Snake2] = 
    moves.foldLeft(List(snake)) { case (snake :: oldSnakes, next) => 
      snake.move(next) :: snake :: oldSnakes
    }.reverse

  def parseLine(line: String): List[Direction] =
    line match {
      case s"U ${value}" => List.fill(value.toInt)(Up)
      case s"D ${value}" => List.fill(value.toInt)(Down)
      case s"R ${value}" => List.fill(value.toInt)(Right)
      case s"L ${value}" => List.fill(value.toInt)(Left)
      case _ => throw new IllegalArgumentException()
    }

  def contract(int: Int): Int =
    if (int < 0) int + 1
    else if (int > 0) int - 1
    else 0

  val moves = data.flatMap(parseLine)
  val snakes = applyMoves(Snake(0, 0, 0, 0), moves)
  val count = snakes.map(s => (s.tailX, s.tailY)).toSet.size

  val snakes2 = applyMoves2(Snake2(0, 0, List.fill(9)((0, 0))), moves)
  val count2 = snakes2.map(_.tail(8)).toSet.size
  println(count2)
}
