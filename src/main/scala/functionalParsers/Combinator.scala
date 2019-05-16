package functionalParsers

import functionalParsers.Parser.{Failure, Parser, Success}
object Combinator {

  def charWithPridicate(predicate: Char => Boolean): Parser[Char] = input => {
    if (!input.isEnd) {
      if (predicate(input.current)) {
        Success(input.current, input.next)
      } else Failure("Failed")
    } else {
      Failure("End of string")
    }
  }

  def char(c: Char) = charWithPridicate(_ == c)

  def digit(c: Char) = charWithPridicate(_.isDigit)

  def anyChar() = charWithPridicate(_ => true)

  def newLine(): Parser[Seq[Char]] = stringParser(System.lineSeparator())

  def stringParser(charSeq: String): Parser[Seq[Char]] = {
    val charParsingResult: Seq[Parser[Char]] = charSeq.map(char)
    val result: Parser[Seq[Char]] = ParserExtensions.sequence(charParsingResult) //TODO: sequence as extension
    result
  }

}
