import scala.util.matching.Regex
import scala.io.Source
import scala.math

case class Card(
  winningNumbers: Seq[Int],
  myNumbers: Seq[Int],
):
  val matches = winningNumbers.intersect(myNumbers).length
  val score = math.pow(2, matches - 1).toInt

object Card:
  val numRegex = "(\\d+)".r

  def fromLine = (line: String) =>
    val allNumbers = line.split(":")(1).split("\\|")
    val winningNumbers = numRegex.findAllIn(allNumbers(0)).toList.map(_.toInt)
    val myNumbers = numRegex.findAllIn(allNumbers(1)).toList.map(_.toInt)
    Card(winningNumbers, myNumbers)


object Scratchcards:
  def one =
    val cards = parse("04/input.txt")
    cards.map(_.score).sum

  def two =
    val allCards = parse("04/input.txt")
    countScratchcards(allCards, for card <- allCards yield 1)

  def countScratchcards(cards: Seq[Card], counts: List[Int], current: Int = 0): Int =
    if current == cards.length then return counts.sum
    val cardMatches = cards(current).matches
    val copiesWonIndexes = for i <- 1 to cardMatches yield i
    val updatedCounts = copiesWonIndexes.foldLeft(counts) { (acc, i) =>
      acc.updated(current + i, acc(current + i) + counts(current))
    }
    countScratchcards(cards, updatedCounts, current + 1)

  def parse = (fileName: String) =>
    val lines = Source.fromFile(fileName).getLines.toList
    lines.map(Card.fromLine)

Scratchcards.one
Scratchcards.two
