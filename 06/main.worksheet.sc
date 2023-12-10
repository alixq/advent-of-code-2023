import scala.util.matching.Regex
import scala.io.Source
import scala.collection.immutable.Range

case class Race(time: BigInt, record: BigInt):
  def waysToBeat =
    Range.BigInt(0, time + 1, 1).count{ holdTime =>
      (time - holdTime) * holdTime > record
    }

object WaitForIt:
  def one =
    val lines = readFile("06/input.txt")
    val times = "\\d+".r.findAllIn(lines(0)).map(BigInt(_))
    val distances = "\\d+".r.findAllIn(lines(1)).map(BigInt(_))
    val races = times.zip(distances)
      .map{ (time, distance) => Race(time, distance) }
    races.foldLeft(1) { (acc, el) => acc * el.waysToBeat }

  def two =
    val lines = readFile("06/input.txt")
    val time = BigInt("\\d+".r.findAllIn(lines(0)).map(_.toString).mkString)
    val distance = BigInt("\\d+".r.findAllIn(lines(1)).map(_.toString).mkString)
    Race(time, distance).waysToBeat

  def readFile(fileName: String) =
    Source.fromFile(fileName).getLines.toList



WaitForIt.one
WaitForIt.two
