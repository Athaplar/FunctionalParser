package functionalParsers.applications

import functionalParsers.Combinator._
import functionalParsers.Parser
import functionalParsers.ParserExtensions._
/*CSV parser

1. Build Parser for parsing CsvValue
2. Parser for csv separator
3. Parser for newline
4. Build the Or combinator - done
5. Combinator for Text - done
6. Combinator for end() - done


 */
object CsvParser {

  //CSV Line is made of csvSeparator, content , csvseparator. many..end of Line.
  //You need many lines of csv Line.

  type Line = Seq[String]
  val DeLimiter = ','

  /**
    *
    *
    * @return
    */
  def csvParser(): Parser[Seq[Line]] = csvLine().many()

  def csvLine(): Parser[Line] =
    for {
      value <- csvValue().many()
      _ <- newLine()
    } yield value

  def csvValue(): Parser[String] =
    toText(deLimitedCsvValue() or nonDelimitedValue()) //TODO: Convert toText to Parser extension method.

  def deLimitedCsvValue() = {
    (for {
      _ <- csvDelimiter()
      value <- anyChar().except(DeLimiter).many()
      _ <- csvDelimiter()
    } yield value)
      .or(
        for {
          value <- anyChar().except(DeLimiter).many()
          _ <- csvDelimiter()
        } yield value
      )
  } //TODO: Simplify logic

  def nonDelimitedValue(): Parser[Seq[Char]] =
    anyChar().except(DeLimiter).many() // TODO: add except line separator
  def csvDelimiter(): Parser[Char] = charParser(DeLimiter)

}
