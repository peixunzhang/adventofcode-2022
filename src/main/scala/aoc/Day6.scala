package aoc
import better.files.Resource

object Day6 extends App {
  val data = Resource.getAsString("day6/real.txt").toList
  def solve(data: List[Char], window: List[Char] = List.empty, acc: Int = 0): Int = {
    data match {
      case x :: xs => 
        if (window.length < 14) solve(xs, x :: window, acc+1)
        else if (window == window.distinct) acc
        else solve(xs, x :: window.init, acc+1)
      case Nil =>
        if (window == window.distinct) acc else -1
    }
  }
println(solve(data))
}
