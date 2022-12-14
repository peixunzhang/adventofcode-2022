package aoc
import better.files.Resource

object Day3 extends App {
  val data = Resource.getAsString("day3/real.txt").split("\n").toList
  def splitTo2(data: List[String]): List[(String, String)] = data.map{ item => 
    item.splitAt(item.length()/2)
  }

  def getDupilcate(first: String, second: String): Set[Char] = {
    val secondSet = second.toSet
    first.foldLeft(Set.empty[Char])((acc, next) => if (secondSet.contains(next)) acc+next else acc)
  } 

  val chars = splitTo2(data).flatMap{case (first, second) => getDupilcate(first, second)}

  def toInt(data: Char): Int = if (data.isLower) data.toInt-96 else data.toInt - 38

  val sum = chars.map(toInt).sum
  // println(sum)

  def groupBy3(data: List[String], acc: List[(String, String, String)] = Nil): List[(String, String, String)] = data match {
    case Nil => acc
    case x :: x2 :: x3 :: xs => groupBy3(xs, (x, x2, x3) :: acc)
    case _ => throw new IllegalStateException
  }

  val groupOf3String = groupBy3(data)

  def solve2(first: String, second: String, third: String): Set[Char] = {
    val secondSet = second.toSet
    val thirdSet = third.toSet
    first.foldLeft(Set.empty[Char])((acc, next) => if (secondSet.contains(next) && thirdSet.contains(next)) acc+next else acc)
  }
  val secondPartChars = groupOf3String.flatMap{case (first, second, third) => solve2(first, second, third)}

  val secondSum = secondPartChars.map(toInt).sum
  println(secondSum)
}
  
