import scala.util.matching.Regex
import scala.io.Source

case class Step(init: String):
  val hash =
    init.foldLeft(0) {
      case (acc, char) => (acc + char.toInt) * 17 % 256
    }

case class Lens(label: String, var value: Int)

case class Box(var lenses: List[Lens])

object LensLibrary:
  def one =
    val steps = parse("15/input.txt")
    steps.map(Step(_).hash).sum

  def two =
    val instructions = parse("15/input.txt")
    var boxes = Array.fill[Box](256)(Box(List.empty))

    for inst <- instructions do
      inst match
        case s"$label=$value" =>
          val box = boxes(Step(label).hash)
          box.lenses.find(_.label == label) match
            case Some(lens) => lens.value = value.toInt
            case None => box.lenses = box.lenses :+ Lens(label, value.toInt)
        case s"$label-" =>
          val box = boxes(Step(label).hash)
          box.lenses = box.lenses.filter(_.label != label)

    val lenses =
      for (box, boxIndex) <- boxes.zipWithIndex
        (lens, lensIndex) <- box.lenses.zipWithIndex
      yield (1 + boxIndex) * (1 + lensIndex) * lens.value

    lenses.sum

  def parse(fileName: String) =
    readFile(fileName).head.split(",")

  def readFile(fileName: String) =
    Source.fromFile(fileName).getLines.toList

LensLibrary.one
LensLibrary.two
