import scala.util.matching.Regex
import scala.io.Source
import scala.math
import scala.collection.immutable.Range

case class MapRange(
  sourceStart: BigInt,
  destinationStart: BigInt,
  length: BigInt
)

case class Map(mapRanges: Seq[MapRange]):
  def getMatch(in: BigInt): BigInt =
    mapRanges.collectFirst{
      case MapRange(src, dst, len) if in >= src && in < src + len =>
        in + dst - src
    }.getOrElse(in)

case class Almanac(maps: Seq[Map]):
  def mapInputs(inputs: Iterator[BigInt], currentIndex: Int): Iterator[BigInt] =
    if currentIndex == maps.length then return inputs
    val currentMap = maps(currentIndex)
    val matches = inputs.map(currentMap.getMatch(_))
    mapInputs(matches, currentIndex + 1)

  def getLocations(seeds: Iterator[BigInt]) = mapInputs(seeds, 0)

case object Almanac:
  def fromLines(lines: Seq[String]) =
    val mapLines = lines.zipWithIndex.collect {
      case (line, i) if ".+map\\:".r.matches(line) => i
    }
    val mapCount = mapLines.length

    val maps = for (currentLine, i) <- mapLines.zipWithIndex yield {
      val nextMapLine =
        if mapCount > i + 1 then mapLines(i + 1) else lines.length
      val mapRangesLines = lines.slice(currentLine + 1, nextMapLine)
      val mapRanges = for line <- mapRangesLines yield {
        val m = "(\\d+) (\\d+) (\\d+)".r.findFirstMatchIn(line).get
        MapRange(BigInt(m.group(2)), BigInt(m.group(1)), BigInt(m.group(3)))
      }
      Map(mapRanges)
    }
    Almanac(maps)

object Seeds:
  def one =
    val (seedNumbers, filteredLines) = parse
    val almanac = Almanac.fromLines(filteredLines)
    almanac.getLocations(seedNumbers).min

  def two =
    val (seedNumbers, filteredLines) = parse
    val allSeeds = seedNumbers.grouped(2).map { couple =>
      Range.BigInt(couple(0), couple(0) + couple(1), 1)
    }.flatten
    val almanac = Almanac.fromLines(filteredLines)
    almanac.getLocations(allSeeds).min

  def parse =
    val lines = Source.fromFile("05/input.txt").getLines.toList
    val seedNumbers = "\\d+".r.findAllIn(lines(0)).map(BigInt(_))
    val filteredLines = lines.tail.filter(_ != "")
    (seedNumbers, filteredLines)


Seeds.one
// Seeds.two
