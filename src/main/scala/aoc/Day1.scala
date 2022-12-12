package aoc

import better.files.Resource

object Day1 extends App {
  val data = Resource.getAsString("day1/real.txt")
  def solve(data: String): List[Int] = {
    val (list, last) = data.split("\n").toList.foldLeft((List.empty[Int], 0)) {
      case ((result, acc), elem) =>
        if (elem.isEmpty()) (acc :: result, 0)
        else (result, elem.toInt + acc)
    }
    last :: list
  }
  println(solve(data).sortBy(-_).take(3).sum)
}
