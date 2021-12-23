import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

//  Day 2 solution

object ParseCourse extends  App {
  val inputExample: List[String] = Using(Source.fromFile("src/main/resources/ParseCourse.example1")) {
    _.getLines().toList
  }.getOrElse(List[String]())

  val inputPart1: List[String] = Using(Source.fromFile("src/main/resources/ParseCourse.input")) {
    _.getLines().toList
  }.getOrElse(List[String]())



  enum Direction:
    case Forward, Up, Down

  case class CourseDirection(direction: Direction, amount: Int)
  case class Location(horizontal: Int, depth: Int)

  final def parseLine(line: String): CourseDirection = {
    val directionRe = raw"([a-z]+) (\d+)".r
    line match {
      case directionRe(dir, amount) =>
        dir match {
          case "forward" => CourseDirection(Direction.Forward, amount.toInt)
          case "down" => CourseDirection(Direction.Down, amount.toInt)
          case "up" => CourseDirection(Direction.Up, amount.toInt)
          case _ => CourseDirection(Direction.Forward, 0)
        }
      case _ => CourseDirection(Direction.Forward, 0)
    }

  }

  final def ParseDirection(direction: CourseDirection, curLocation: Location): Location =
    direction match {
      case CourseDirection(Direction.Forward, x) => Location(horizontal = curLocation.horizontal + x, curLocation.depth)
      case CourseDirection(Direction.Down, x) => Location(horizontal = curLocation.horizontal, curLocation.depth + x)
      case CourseDirection(Direction.Up, x) => Location(horizontal = curLocation.horizontal, curLocation.depth - x)
    }

  @tailrec
  final def applyDirections(initial: Location, directions: List[CourseDirection]): Location =
    directions match {
      case head::tail => applyDirections(ParseDirection(head, initial), tail)
      case Nil => initial
    }

  case class PartTwoLocation(horizontal: Int, depth: Int, aim: Int)
  final def ParsePartTwoDirection(direction: CourseDirection, curLocation: PartTwoLocation): PartTwoLocation =
    direction match {
      case CourseDirection(Direction.Forward, x) => PartTwoLocation(horizontal = curLocation.horizontal + x,
        depth = curLocation.depth + curLocation.aim * x,
        aim = curLocation.aim)
      case CourseDirection(Direction.Down, x) => PartTwoLocation(horizontal = curLocation.horizontal,
        depth = curLocation.depth,
        aim = curLocation.aim + x)
      case CourseDirection(Direction.Up, x) => PartTwoLocation(horizontal = curLocation.horizontal,
        depth = curLocation.depth,
        aim = curLocation.aim - x)
    }

  @tailrec
  final def applyPartTwoDirections(initial: PartTwoLocation, directions: List[CourseDirection]): PartTwoLocation =
    directions match {
      case head::tail => applyPartTwoDirections(ParsePartTwoDirection(head, initial), tail)
      case Nil => initial
    }

  val parsedExample = inputExample.map(parseLine)
  val parsedInput = inputPart1.map(parseLine)
  val exampleLocation = applyDirections(Location(0, 0), parsedExample)
  val inputLocation = applyDirections(Location(0, 0), parsedInput)
  println(s"Final location (example) = ${exampleLocation.horizontal * exampleLocation.depth}")
  println(s"Final location (intpu) = ${inputLocation.horizontal * inputLocation.depth}")

  val examplePartTwoLocation = applyPartTwoDirections(PartTwoLocation(0, 0, 0), parsedExample)
  val inputPartTwoLocation = applyPartTwoDirections(PartTwoLocation(0, 0, 0), parsedInput)
  println(s"Final location (example) = ${examplePartTwoLocation.horizontal * examplePartTwoLocation.depth}")
  println(s"Final location (intpu) = ${inputPartTwoLocation.horizontal * inputPartTwoLocation.depth}")

}
