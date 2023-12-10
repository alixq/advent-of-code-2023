import scala.collection.mutable
import scala.collection.mutable
import scala.util.matching.Regex
import scala.io.Source

def gcd(a: BigInt, b: BigInt): BigInt = if b == 0 then a else gcd(b, a % b)

def lcm(list: Seq[BigInt]) = list.foldLeft(1:BigInt) {
  (acc, n) => acc * n / gcd(n, acc)
}

enum Direction:
  case Left, Right

case class Node(loc: String, left: String, right: String)

case class Network(directions: Seq[Direction], nodes: Seq[Node]):
  val indexed = nodes.map(n => (n.loc -> n)).toMap

  private def countSteps(loc: String, stepCount: Int = 0): Int =
    if loc == "ZZZ" then return stepCount
    countSteps(nextNode(loc, stepCount), stepCount + 1)

  def stepCount = countSteps("AAA")

  private def countSwooshes(loc: String, count: Int = 0): Int =
    if loc.endsWith("Z") then return count
    countSwooshes(nextNode(loc, count), count + 1)

  def swooshCount = lcm(nodes.collect {
    case node if node.loc.endsWith("A") => BigInt(countSwooshes(node.loc))
  })

  private def nextNode(current: String, count: BigInt) =
    directions((count % directions.length).toInt) match
      case Direction.Left => indexed(current).left
      case Direction.Right => indexed(current).right

object HauntedWasteland:
  def one =
    val network = parse("08/input.txt")
    network.stepCount

  def two =
    val network = parse("08/input.txt")
    network.swooshCount

  def parse(fileName: String) =
    val lines = readFile(fileName).filter(_ != "")
    val directions = for char <- lines.head
      yield if char == 'L' then Direction.Left else Direction.Right
    val nodes = for line <- lines.tail yield {
      val m = "([0-9A-Z]+) = \\(([0-9A-Z]+), ([0-9A-Z]+)\\)".r.findFirstMatchIn(line).get
      Node(m.group(1), m.group(2), m.group(3))
    }
    Network(directions, nodes)

  def readFile(fileName: String) =
    Source.fromFile(fileName).getLines.toList

HauntedWasteland.one
HauntedWasteland.two
