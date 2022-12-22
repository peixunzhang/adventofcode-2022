package aoc

import better.files.Resource

object Day5 extends App {
  type State = Vector[List[Char]]
  type Action = (Int, Int, Int)

  val data = Resource.getAsString("day5/real.txt").split("\n").toList

  def parseState(data: List[String]): (State, List[String]) = {
    val rawState = data.takeWhile(_.nonEmpty)
    val vectorSize = (rawState.tail.length+1)/4
    val lessRawState = rawState.init.map(parseStateLine(_, vectorSize))
    val finalState = lessRawState.transpose.map(_.flatten).toVector
    val leftOver = data.dropWhile(_.nonEmpty).tail
    (finalState, leftOver)
  }

  def parseAction(data: String): Option[Action] = {
    val pattern = raw"move (\d+) from (\d+) to (\d+)".r
    data match {
      case pattern(index, from, to) => Some((index.toInt, from.toInt-1, to.toInt-1))
      case _ => None
    }
  }

  def parseActions(data: List[String]): List[Action] = data.flatMap(parseAction(_))

  def parseAll(data: List[String]): (State, List[Action]) = {
    val (state, rawActions) = parseState(data)
    val actions = parseActions(rawActions)
    (state, actions)
  }
  val (state, actions) = parseAll(data)

  def parseStateLine(data: String, size: Int): Vector[Option[Char]] = {
    data.grouped(4).map(_.toList match {
        case '[' :: box :: ']' :: _ => Some(box)
        case _ => None
    }).toVector.padTo(size, None)
  } 

  def moveBox(state: State, action: Action): State = {
    val (index, from, to) = action
    index match {
      case 0 => state
      case _ => 
        val oldFrom = state(from)
        val oldTo = state(to)
        val step1 = state.updated(from, oldFrom.tail)
        val step2 = step1.updated(to, oldFrom.head :: oldTo)
        moveBox(step2, (index-1, from, to))
    }
  }

  def moveBox2(state: State, action: Action): State = {
    val (count, from, to) = action
    val oldFrom = state(from)
    val oldTo = state(to)
    val step1 = state.updated(from, oldFrom.drop(count))
    val step2 = step1.updated(to, oldFrom.take(count) ++ oldTo)
    step2
    
  }

  def applyActions(state: State, actions: List[Action]): State = {
    // movebox(movebox(state, actions(0)), actions(1))
    actions.foldLeft(state)(moveBox2(_, _))
  }

  def getChars(finalState: State): String = {
    finalState.map(_.head).mkString
  }
  println(getChars(applyActions(state, actions)))
}
