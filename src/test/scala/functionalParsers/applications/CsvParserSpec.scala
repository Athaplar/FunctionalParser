package functionalParsers.applications

import functionalParsers.Parser
import functionalParsers.Parser.Input
import functionalParsers.applications.CsvParser._
import org.scalatest.FlatSpec
class CsvParserSpec extends FlatSpec {

  //TODO: csv edge cases

  "A csvline without newline" should "get parsed" in {
    val valueParser: Parser[String] = csvValue()
    val input = Input("x,y,z")
    println(valueParser(input))
  }

  "A csv Line ending with newline" should "get parsed" in {

    val csvParser = CsvParser.csvParser()
    val input = Input("x,y,z" + System.lineSeparator())
    println(csvParser(input))

  }

  " 2 csv Line" should "get parsed" in {

    val csvParser = CsvParser.csvParser()
    val input = Input("x,y,z" + System.lineSeparator() + "a,b,c")
    println(csvParser(input))
  }

}
