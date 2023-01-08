package aoc
import better.files.Resource
import scala.util.parsing.combinator.RegexParsers

object Day13 extends App {
  val data = Resource.getAsString("day13/real.txt")

  sealed trait Value {
    def compare(other: Value): Order = {
      (this, other) match {
        case (JustInt(left), JustInt(right)) => if (left < right) Smaller else if (left > right) Bigger else DontKnow
        case (ListOfValue(left), ListOfValue(right)) => 
          def go(left: List[Value], right: List[Value]): Order = 
            (left, right) match {
              case (x :: xs, y :: ys) => x.compare(y) >> go(xs, ys)
              case (Nil, Nil) => DontKnow
              case (Nil, _) => Smaller
              case (_, Nil) => Bigger
            }
            go(left, right)
        case (JustInt(_), ListOfValue(_)) => 
          ListOfValue(List(this)).compare(other)
        case (ListOfValue(_), JustInt(_)) => 
          compare(ListOfValue(List(other)))
    }
   }

   override def toString(): String = this match {
    case JustInt(value) => value.toString()
    case ListOfValue(values) => values.map(_.toString()).mkString("[", ",", "]")
   }
  }
  final case class ListOfValue(values: List[Value]) extends Value
  final case class JustInt(value: Int) extends Value

  object Value {
    implicit val ordering: Ordering[Value] = new Ordering[Value] {
      def compare(x: Value, y: Value): Int = x.compare(y) match {
        case Bigger => 1
        case DontKnow => 0
        case Smaller => -1
      }
    }
  }

  sealed trait Order {
    def >>(other: => Order): Order = this match {
      case Bigger => Bigger
      case Smaller => Smaller
      case DontKnow => other
    }
  }
  case object Smaller extends Order
  case object Bigger extends Order
  case object DontKnow extends Order


  object parsing extends RegexParsers {
    val number = "(0|[1-9]\\d*)".r ^^ { _.toInt }

    val justInt = number ^^ JustInt.apply
    val value: Parser[Value] = justInt | listOfValue
    val listOfValue = "[" ~ (value ~ ",").* ~ value.? ~ "]" ^^ { case _ ~ values ~ lastValue ~ _ => ListOfValue(values.map(_._1) ++ lastValue.toList)}
    
    val pairOfValues = listOfValue ~ listOfValue ^^ { case first ~ second => (first, second) }
    val listOfPairs = pairOfValues.*
    val listOfValues = listOfValue.*
  }
  val pairs = parsing.parse(parsing.listOfPairs, data).get
  val result = pairs.map{case (first, second) => first.compare(second)}
  val count = result.zipWithIndex.collect{case (Smaller, n) => n+1}.sum

  // --------part2-----

  val values2: List[Value] = parsing.parse(parsing.listOfValues, data).get
  val divider1 = ListOfValue(List(ListOfValue(List(JustInt(2)))))
  val divider2 = ListOfValue(List(ListOfValue(List(JustInt(6)))))
  val sorted = (divider1 :: divider2 :: values2).sorted
  val score = (sorted.indexOf(divider1)+1) * (sorted.indexOf(divider2)+1)
  
  
  println(score)
}
