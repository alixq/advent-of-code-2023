import scala.collection.immutable.NumericRange.Inclusive
import scala.util.matching.Regex
import scala.io.Source

case class Num(value: Int, line: Int, cols: Tuple2[Int, Int]):
  val occupiedCoords = Inclusive(cols._1, cols._2, 1).map((line, _))

case class Symbol(star: Boolean, line: Int, col: Int):
  private val adjacentCoords = Inclusive(line - 1, line + 1, 1) flatMap { l =>
    Inclusive(col - 1, col + 1, 1) map { c => (l, c) }
  }

  val adjacentNums = (nums: Seq[Num]) => nums.filterNot { num =>
    adjacentCoords.intersect(num.occupiedCoords).isEmpty
  }

case class Engine(
  var numbers: Seq[Num] = Seq[Num](),
  var symbols: Seq[Symbol] = Seq[Symbol](),
):
  def add = (element: Num | Symbol) =>
    element match
      case num: Num => numbers = numbers.appended(num)
      case symbol: Symbol => symbols = symbols.appended(symbol)

  def partNumbers =
    symbols.flatMap(_.adjacentNums(numbers)).distinct

  def gearRatios = symbols.collect {
      case sym: Symbol if sym.star =>
        val adj = sym.adjacentNums(numbers)
        adj.length match {
          case 2 => Some(adj(0).value * adj(1).value)
          case _ => None
        }
    }.flatten

object Engine:
  val regex = "(\\d+)|([^\\d.])".r

  def fromLines = (lines: Seq[String]) =>
    val engine = Engine()
    lines.zipWithIndex.map {
      case (element, lineIndex) =>
        val matches = regex.findAllMatchIn(element).toList.foreach { m =>
          m.matched.toIntOption match
            case Some(value) => engine.add(Num(value, lineIndex, (m.start, m.end - 1)))
            case None => engine.add(Symbol(m.matched == "*", lineIndex, m.start))
        }
    }
    engine


object GearRatios:
  val one =
    val engine = parse("03/input.txt")
    engine.partNumbers.map(_.value).sum

  val two =
    val engine = parse("03/input.txt")
    engine.gearRatios.sum

  def parse = (fileName: String) =>
    val lines = Source.fromFile(fileName).getLines.toList
    Engine.fromLines(lines)

GearRatios.one
GearRatios.two
