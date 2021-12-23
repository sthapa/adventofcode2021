import scala.io.Source
import scala.util.Using

//  Day 3 solution


object BinaryDiagnostic extends App {
  val inputExample: List[String] = Using(Source.fromFile("src/main/resources/BinaryDiagnostic.example1")) {
    _.getLines().toList
  }.getOrElse(List[String]())

  val inputPart1: List[String] = Using(Source.fromFile("src/main/resources/BinaryDiagnostic.input")) {
    _.getLines().toList
  }.getOrElse(List[String]())

  final def mostCommonDigit(digits: List[Char]): Char = {
    val groupedDigits =
      digits.groupBy(x => x)
        .map(x => (x._1, x._2.length))
        .maxBy(_._2)
    groupedDigits(0)
  }

  final def getGammaRate(values: List[String]): Int = {
    val digitList = for {
      i <- 0 until values.head.length
    } yield {
      val digits = values.map(x => x.charAt(i))
      mostCommonDigit(digits)
    }
    Integer.parseInt(digitList.mkString, 2)
  }

  final def calculatePower(gammaRate: Int, mask: Int): Int =
    gammaRate * (mask ^ gammaRate)


  val exampleGamma = getGammaRate(inputExample)
  println(s"gamma rate = $exampleGamma")
  println(s"power = ${calculatePower(exampleGamma, 0x1F)}")

  val inputGamma = getGammaRate(inputPart1)
  println(s"gamma rate = $inputGamma")
  println(s"power = ${calculatePower(inputGamma, mask = 0xFFF)}")

  // Part 2 solution

}
