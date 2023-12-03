import scala.util.matching.Regex
import scala.io.Source

case class ColorCount(red: Int, green: Int, blue: Int):
  def contains = (other: ColorCount) =>
    red >= other.red && green >= other.green && blue >= other.blue

object ColorCount:
  val regex = "(\\d+)\\s(red|green|blue)".r

  def fromString = (round: String) =>
    var red, green, blue: Int = 0
    round.split(",") map { colorWithCount =>
      regex.findFirstMatchIn(colorWithCount).get.subgroups match {
        case n :: "red" :: _ => red = n.toInt
        case n :: "blue" :: _ => blue = n.toInt
        case n :: "green" :: _ => green = n.toInt
        case _ =>
      }
    }
    ColorCount(red, green, blue)

case class Game(id: Int, colorCounts: Seq[ColorCount]):
  val power =
    colorCounts.map(_.red).max *
    colorCounts.map(_.green).max *
    colorCounts.map(_.blue).max

object Game:
  def fromString = (line: String) =>
    val splitText = line.split(":")
    val gameId = "\\d+".r.findFirstIn(splitText(0)).get.toInt
    val colorcounts = splitText(1).split(";") map { ColorCount.fromString(_) }
    Game(gameId, colorcounts.toList)


object CubeConundrum:
  def one =
    val availableCount = ColorCount(12, 13, 14)
    val workingGames = parseGames("02/input.txt").collect {
      case game if game.colorCounts.forall { availableCount.contains(_) } => game.id
    }
    println(workingGames.sum)

  def two =
    val gamePowers = parseGames("02/input.txt").map(_.power)
    println(gamePowers.sum)

  def parseGames = (fileName: String) =>
    Source.fromFile(fileName).getLines map { Game.fromString(_) }

CubeConundrum.one
CubeConundrum.two
