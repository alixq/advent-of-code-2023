import scala.util.matching.Regex
import scala.io.Source

object Trebuchet:
  def one = common("01/input.txt", (line) => "\\d".r.findAllIn(line).toList)

  def two =
    val unionRegex = "(\\d)|(one)|(two)|(three)|(four)|(five)|(six)|(seven)|(eight)|(nine)"
    val withOverlapping = ("(?=(" + unionRegex + ")).").r

    common("01/input.txt", (line) =>
      withOverlapping.findAllMatchIn(line).map(_.subgroups(0)).toList
    )

  def common = (fileName: String, getMatches: Function1[String, List[String]]) =>
    val lines = Source.fromFile(fileName).getLines.toList
    val numbers = lines map { line =>
      val digits = getMatches(line)
      stringToInt(digits.head) * 10 + stringToInt(digits.last)
    }
    println(numbers.sum)

  def stringToInt(word: String) =
    word.toIntOption getOrElse {
      word match {
        case "one" => 1
        case "two" => 2
        case "three" => 3
        case "four" => 4
        case "five" => 5
        case "six" => 6
        case "seven" => 7
        case "eight" => 8
        case "nine" => 9
      }
    }

Trebuchet.one
Trebuchet.two
