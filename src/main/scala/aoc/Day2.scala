package aoc
import better.files.Resource

//A for Rock, B for Paper, and C for Scissors
//X for Rock, Y for Paper, and Z for Scissors
//1 for Rock, 2 for Paper, and 3 for Scissors
//0 if you lost, 3 if the round was a draw, and 6 if you won

object Day2 extends App {
  val data = Resource.getAsString("day2/real.txt").split("\n").toList
  def solve(data: List[String], acc: Int = 0): Int = data match {
    case Nil         => acc
    case "A X" :: xs => solve(xs, acc + 4)
    case "A Y" :: xs => solve(xs, acc + 8)
    case "A Z" :: xs => solve(xs, acc + 3)
    case "B X" :: xs => solve(xs, acc + 1)
    case "B Y" :: xs => solve(xs, acc + 5)
    case "B Z" :: xs => solve(xs, acc + 9)
    case "C X" :: xs => solve(xs, acc + 7)
    case "C Y" :: xs => solve(xs, acc + 2)
    case "C Z" :: xs => solve(xs, acc + 6)
    case _ :: xs     => solve(xs, acc + 0)
  }
  // println(solve(data))

  // X lose, Y draw, and Z win.
  def solve2(data: List[String], acc: Int = 0): Int = data match {
    case "A X" :: xs => solve2(xs, acc + 0 + 3)
    case "A Y" :: xs => solve2(xs, acc + 3 + 1)
    case "A Z" :: xs => solve2(xs, acc + 6 + 2)
    
    case "B X" :: xs => solve2(xs, acc + 0 + 1)
    case "B Y" :: xs => solve2(xs, acc + 3 + 2)
    case "B Z" :: xs => solve2(xs, acc + 6 + 3)
    
    case "C X" :: xs => solve2(xs, acc + 0 + 2)
    case "C Y" :: xs => solve2(xs, acc + 3 + 3)
    case "C Z" :: xs => solve2(xs, acc + 6 + 1)
    
    case _ :: xs     => solve2(xs, acc + 0)
    case Nil => acc
  }
  println(solve2(data))
}
