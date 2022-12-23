package aoc
import better.files.Resource

object Day7 extends App {
  val data = Resource.getAsString("day7/real.txt").split("\n").toList
  
  sealed trait Node { self =>

    def render: String = {
      def go(node: Node, indent: Int): List[String] = node match {
        case File(_) => Nil
        case Directory(children) =>
          children.flatMap { case (k, v) => 
            s"${" ".repeat(indent)}$k" :: go(v, indent + 2)
          }.toList
      }
      go(self, 0).mkString("\n")
    }
    
  }
  final case class Directory(children: Map[String, Node]) extends Node
  final case class File(size: Int) extends Node

  val emptyDiretory = Directory(Map.empty)

  final case class State(tree: Node, dirStack: List[String]) { self =>

    def addNode(name: String, value: Node): State = {
      def go(tree: Node, location: List[String]): Node =
        (location, tree) match {
          case (Nil, Directory(children)) =>
            Directory(children + (name -> value))
          case (x :: xs, Directory(children)) =>
            val newChild = go(children(x), xs)
            Directory(children + (x -> newChild))
          case _ =>
            throw new IllegalStateException(s"Invalid adding to tree: (${self.tree}, $dirStack)")
        }
      State(go(tree, dirStack.reverse), dirStack)
    }

  }

  object State {
    val initial = State(emptyDiretory, Nil)
  }

  def applyOperation(state: State, data: String): State = data match { 
    case "$ cd /" => state.copy(dirStack = Nil)
    case "$ cd .." => state.copy(dirStack = state.dirStack.tail)
    case s"$$ cd ${name}" => state.copy(dirStack = name :: state.dirStack)
    case "$ ls" => state
    case s"dir ${name}" => state.addNode(name, emptyDiretory)
    case s"${size} ${name}" => state.addNode(name, File(size.toInt))
  }

  def parse(data: List[String]): Node = 
    data.foldLeft(State.initial)(applyOperation(_, _)).tree

  def getSize(data: Node): Int = {
    data match {
      case Directory(children) => children.values.map(getSize(_)).sum
      case File(size) => size
    }
  }

  def sumFolderSizesSmallerThan(node: Node, target: Int): Int = {
    node match {
      case Directory(children) =>
        val sumOfChildren = children.values.map(sumFolderSizesSmallerThan(_, target)).sum
        val ownSize = getSize(node)
        if (ownSize <= target) ownSize + sumOfChildren else sumOfChildren
      case File(_) => 0
    }
  }

  def directorySizes(data: Node, name: String = "/"): List[(String, Int)] = {
    data match {
      case File(size) => Nil
      case Directory(children) => 
        val childSizes = children.flatMap{case (name, child) => directorySizes(child, name)}.toList
        (name, getSize(data)) :: childSizes
    }
  }

  val rootNode = parse(data)

  val unused = 70000000 - getSize(rootNode)
  val needed = 30000000 - unused

  println(directorySizes(rootNode).filter(_._2 >= needed).minBy(_._2)) // faster than sort 
}
