import scala.io.Source
import scala.util.Using

//  Day 1 solution

object SonarSweep extends App {
  val inputExample1: List[Int] = Using(Source.fromFile("src/main/resources/SonarSweep.example1")) {
    _.getLines().toList
  }.getOrElse(List[String]()).map(_.toInt)

  val inputPart1: List[Int] = Using(Source.fromFile("src/main/resources/SonarSweep.input")) {
    _.getLines().toList
  }.getOrElse(List[String]()).map(_.toInt)

  final def increases(depths: List[Int]): Int = {
    val depthPairs = depths.dropRight(1) zip depths.drop(1)
    depthPairs.count((x, y) => y > x)
  }

  final def windowedIncreases(depth: List[Int]): Int = {
    val slidingDepths = depth.sliding(3).map(x => x.sum).toList
    val slidingPairs = slidingDepths.dropRight(1) zip slidingDepths.drop(1)
    slidingPairs.count((x, y) => y > x)
  }
  println(s"Increases in depth (example) = ${increases(inputExample1)}")
  println(s"Increases in depth = ${increases(inputPart1)}")
  println(s"Increases in sliding depth (example) = ${windowedIncreases(inputExample1)}")
  println(s"Increases in sliding depth = ${windowedIncreases(inputPart1)}")


}
