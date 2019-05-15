package functionalParsers.applications

import functionalParsers.Combinator._
import functionalParsers.Parser.{Input, Parser}
import functionalParsers.ParserExtensions._

object CsvParser {

  type Line = Seq[String]
  val DeLimiter = ','

  def csvParser(): Parser[Seq[Line]] = csvLine().many()

  def csvLine(): Parser[Seq[String]] =
    for {
      head <- csvValue()
      rest <- csvDelimiter().flatMap(_ => csvValue()).many()
      _ <- newLine()
    } yield head +: rest

  def csvValue(): Parser[String] =
    anyChar().expect(csvDelimiter()).expect(newLine()).many().text()

  def csvDelimiter(): Parser[Char] = charParser(DeLimiter)
}

object CsvParserTest extends App {
  import CsvParser._

  val input = Input("x,y,z" + System.lineSeparator())
  println(csvParser()(input))

  //type Parser[T] = Input => Result[T]

}
