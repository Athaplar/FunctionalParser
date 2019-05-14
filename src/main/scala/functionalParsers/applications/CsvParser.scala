package functionalParsers.applications

import functionalParsers.Combinator._
import functionalParsers.Parser
import functionalParsers.ParserExtensions._

object CsvParser extends App {

  val result: Seq[String] = "asa" +: Seq("asas")
  println(result)
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
    anyChar().many().expect(csvDelimiter()).expect(newLine()).text()

  def csvDelimiter(): Parser[Char] = charParser(DeLimiter)

}
