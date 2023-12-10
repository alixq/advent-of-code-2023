import scala.util.matching.Regex
import scala.io.Source
import scala.util.Sorting

object Card:
  val order = "23456789TJQKA".toCharArray()
  val jokerOrder = "J23456789TQKA".toCharArray()

case class Card(char: Char):
  def compareRegular(that: Card) =
    Card.order.indexOf(char) - Card.order.indexOf(that.char)

  def compareWithJokers(that: Card) =
    Card.jokerOrder.indexOf(char) - Card.jokerOrder.indexOf(that.char)

enum HandType:
  case HighCard, Pair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

case class Hand(cards: Seq[Card], bet: Int):
  val regularHandType =
    val cardCounts = cards.groupBy(identity).view.mapValues(_.size).values.toList
    cardCounts.size match
      case 1 => HandType.FiveOfAKind
      case 2 if cardCounts.contains(4) => HandType.FourOfAKind
      case 2 => HandType.FullHouse
      case 3 if cardCounts.contains(3) => HandType.ThreeOfAKind
      case 3 => HandType.TwoPair
      case 4 => HandType.Pair
      case 5 => HandType.HighCard

  val jokerHandType =
    cards.count(_ == Card('J')) match
      case 0 | 5 =>
        regularHandType
      case 1 =>
        regularHandType match
          case HandType.FourOfAKind => HandType.FiveOfAKind
          case HandType.ThreeOfAKind => HandType.FourOfAKind
          case HandType.TwoPair => HandType.FullHouse
          case HandType.Pair => HandType.ThreeOfAKind
          case _ => HandType.Pair
      case 2 =>
        regularHandType match
          case HandType.FullHouse => HandType.FiveOfAKind
          case HandType.TwoPair => HandType.FourOfAKind
          case _ => HandType.ThreeOfAKind
      case 3 =>
        regularHandType match
          case HandType.FullHouse => HandType.FiveOfAKind
          case _ => HandType.FourOfAKind
      case 4 => HandType.FiveOfAKind

object RegularOrdering extends Ordering[Hand]:
  def compare(a: Hand, b: Hand): Int =
    a.regularHandType.ordinal - b.regularHandType.ordinal match
      case 0 =>
        a.cards.zip(b.cards).collectFirst {
          case (a, b) if a.compareRegular(b) != 0 => a.compareRegular(b)
        }.getOrElse(0)
      case n => n

object JokerOrdering extends Ordering[Hand]:
  def compare(a: Hand, b: Hand): Int =
    a.jokerHandType.ordinal - b.jokerHandType.ordinal match
      case 0 =>
        a.cards.zip(b.cards).collectFirst {
          case (a, b) if a.compareWithJokers(b) != 0 => a.compareWithJokers(b)
        }.getOrElse(0)
      case n => n


object CamelCards:
  def one = common(RegularOrdering)
  def two = common(JokerOrdering)

  def common(ordering: Ordering[Hand]) =
    val hands = parse("07/input.txt")
    hands.sorted(ordering).zipWithIndex.foldLeft(0) { (acc, el) =>
      val (hand, i) = el
      acc + (i + 1) * hand.bet
    }

  def parse(fileName: String) =
    val lines = readFile(fileName)
    lines.map { line =>
      val values = line.split("\\s")
      val cards = for char <- values(0) yield Card(char)
      Hand(cards, values(1).toInt)
    }

  def readFile(fileName: String) =
    Source.fromFile(fileName).getLines.toList


CamelCards.one
CamelCards.two
