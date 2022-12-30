package aoc

import better.files.Resource
import scala.util.parsing.combinator.RegexParsers

object Day11 extends App {
  val data = Resource.getAsString("day11/real.txt")

  type MonkeyId = Int
 
  final case class Test(divisor: Int, onTrue: Int, onFalse: Int) {
    def run(value: Long): Int =
      if (value % divisor == 0) onTrue else onFalse
  }

  final case class Monkey(id: Int, items: List[Long], operation: Long => Long, test: Test, inspectCount: Long) {

    def addItem(item: Long): Monkey =
      copy(items = items :+ item)

    // monkeys has to include this monkey
    def runSelf(monkeys: Monkeys): Monkeys = {
      val productOfDivisor = monkeys.map(_.test.divisor).product
      def go(monkey: Monkey, monkeys: Monkeys): Monkeys = monkey.items match {
        case Nil => monkeys
        case x :: xs =>
          val worryLevel = monkey.operation(x) % productOfDivisor
          val nextMonkey = monkey.test.run(worryLevel)

          val updatedMonkey = monkey.copy(items = xs, inspectCount = monkey.inspectCount + 1)

          val updatedMonkeys = monkeys
            .updated(monkey.id, updatedMonkey)
            .updated(nextMonkey, monkeys(nextMonkey).addItem(worryLevel))

          go(updatedMonkey, updatedMonkeys)
      }
      go(this, monkeys)
    }

  }

  type Monkeys = Vector[Monkey]

  object parsing extends RegexParsers {
    val number = "(0|[1-9]\\d*)".r ^^ { _.toInt }
    
    val monkeyId = "Monkey" ~ number ~ ":" ^^ { case _ ~ id ~ _ => id }

    val singleItem = number ^^ { number => List(number.toLong) }
    lazy val manyItems = number ~ "," ~ items ^^ { case number ~ _ ~ items => number.toLong :: items }
    lazy val items: Parser[List[Long]] = manyItems | singleItem
    lazy val startingItems = "Starting items:" ~ items ^^ { case _ ~ items => items }

    val operationTimes = "old *" ~ number ^^ { case _ ~ number => (old: Long) => old * number  }
    val operationSquare = "old * old" ^^ { _ => (old: Long) => old * old }
    val operationAdd = "old +" ~ number ^^ { case _ ~ number => (old: Long) => old + number }
    val operation = operationTimes | operationAdd | operationSquare
    val operationLine = "Operation: new =" ~ operation ^^ { case _ ~ op => op }

    val testDivisor = "Test: divisible by" ~ number ^^ { case _ ~ number => number }
    val testMonkeyTrue = "If true: throw to monkey" ~ number ^^ { case _ ~ number => number }
    val testMonkeyFalse = "If false: throw to monkey" ~ number ^^ { case _ ~ number => number }

    val test = testDivisor ~ testMonkeyTrue ~ testMonkeyFalse ^^ { 
      case divisor ~ onTrue ~ onFalse => Test(divisor, onTrue, onFalse)
    }

    val monkey = monkeyId ~ startingItems ~ operationLine ~ test ^^ { 
      case id ~ items ~ operation ~ test => Monkey(id, items, operation, test, 0)    
    }

    val monkeys = monkey.*
  }

  def performRound(monkeys: Monkeys): Monkeys =
    monkeys.indices.foldLeft(monkeys) { case (acc, next) => acc(next).runSelf(acc) }

  def performRounds(monkeys: Monkeys, rounds: Int): Monkeys =
    if (rounds <= 0) monkeys else performRounds(performRound(monkeys), rounds - 1)

  val monkeys = parsing.parseAll(parsing.monkeys, data).get.toVector
  println(performRounds(monkeys, 10000).map(_.inspectCount).sortBy(-_).take(2).product)

  // x % 19 == 0
  // (x - 19)
}
