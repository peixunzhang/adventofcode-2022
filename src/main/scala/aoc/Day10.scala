package aoc
import better.files.Resource

object Day10 extends App {
  val data = Resource.getAsString("day10/real.txt").split("\n").toList

  sealed trait Order
  final case object Noop extends Order
  final case class Addx(value: Int) extends Order

  def parseInstruction(data: String): Order = data match {
    case "noop" => Noop
    case s"addx $value" => Addx(value.toInt)
  }

  def parse(data: List[String], acc: List[Order] = Nil): List[Order] = data match {
    case Nil => acc.reverse
    case x :: xs => parse(xs, parseInstruction(x) :: acc)
  }

  def run(data: List[Order], cycles: Int, acc: Int = 1): Int = data match {
    case Nil => acc
    case Noop :: xs => if (cycles <= 1) acc else run(xs, cycles-1, acc)
    case Addx(value) :: xs => if (cycles <= 2) acc else run(xs, cycles-2, acc + value)
  }
  
  val orders = parse(data)
  def getSignal(cycles: Int) = run(orders, cycles) * cycles

  //Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles.
  val part1 = List(20, 60, 100, 140, 180, 220).map(getSignal(_)).sum

  def isLit(horizontalPosition: Int, registerValue: Int): Boolean =
    (registerValue - horizontalPosition).abs <= 1

  val display = for {
    row <- 0 until 6
    col = for {
            col <- 0 until 40
            register = run(orders, (row * 40) + col + 1)
            symbol = if (isLit(col, register)) "#" else " "
          } yield symbol
  } yield col

  println(display.map(_.mkString).mkString("\n"))

}
