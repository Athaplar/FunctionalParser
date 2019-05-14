package functionalParsers.applications

import functionalParsers.Parser
import functionalParsers.Parser.Input
import functionalParsers.ParserExtensions._
import functionalParsers.applications.CsvParser._
import org.scalatest.FlatSpec
class CsvParserSpec extends FlatSpec {

  //TODO: csv edge cases

  "csvValue x,y" should "get parsed" in {
    val valueParser: Parser[String] = csvValue()
    val input = Input(",x,y,z")
    println(valueParser.many()(input))
    // println(deLimitedCsvValue()(input))
  }

  "\"x,y,z\" csv Line" should "get parsed" in {

    val csvParser = csvValue().many()
    val input = Input(",x,y,z" + System.lineSeparator())

    println(csvParser(input))
    //Get the first Line and iterate Line to assert [0] - z, [1] - y and [2]- z

  }

}
