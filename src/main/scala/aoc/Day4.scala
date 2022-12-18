package aoc

import better.files.Resource

object Day4 extends App {
    val data = Resource.getAsString("day4/real.txt").split("\n").toList
    def getInt(data: String): (Int, Int, Int, Int) = {
      data.split(",").flatMap(_.split("-")).toList.map(_.toInt) match {
        case fs :: fe :: ss :: se :: Nil => (fs, fe, ss, se)
        case _ => throw new IllegalStateException
      }
    }
    def isIn(firstStart: Int, firstEnd: Int, secondStart: Int, secondEnd: Int): Boolean = 
      ((firstStart <= secondStart && firstEnd >= secondEnd) || (secondStart <= firstStart && secondEnd >= firstEnd))
    
    val sections = data.map(getInt(_))
    val isCompleteOverlap = sections.map{case (fs, fe, ss, se) => isIn(fs, fe, ss, se)}
    // println(isCompleteOverlap.count(identity))
//---------------
    def isOverlap(firstStart: Int, firstEnd: Int, secondStart: Int, secondEnd: Int): Boolean =
      (firstEnd >= secondStart && firstEnd <= secondEnd) ||
      (secondEnd >= firstStart && secondEnd <= firstEnd) ||
      (firstStart <= secondStart && firstEnd >= secondEnd) ||
      (secondStart <= firstStart && secondEnd >= firstEnd)

      // ((firstStart <= secondStart && firstEnd >= secondEnd) ||
      // (secondStart <= firstStart && secondEnd >= firstEnd) ||
      // (firstStart <= secondStart && firstEnd >= secondStart) ||
      // (firstStart >= secondStart && firstEnd >= secondStart)
      // )
// 2, 8, 3, 9
// 5, 9, 2, 7    
    
    val isOverlaped = sections.map{case (fs, fe, ss, se) => isOverlap(fs, fe, ss, se)}
    println(isOverlaped.count(identity))
}
