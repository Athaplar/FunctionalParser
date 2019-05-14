package functionalParsers

import functionalParsers.Parser.{Failure, Input, Result, Success}
object Combinator {

  def char(predicate: Char => Boolean): Parser[Char] = new Parser[Char] {

    override def apply(input: Input): Result[Char] =
      if (!input.isEnd) {
        if (predicate(input.current)) {
          Success(input.current, input.next)
        } else Failure("Failed")
      } else {
        Failure("End of string")
      }
  }

  def charParser(c: Char) = char(_ == c)

  def digit(c: Char) = char(_.isDigit)

  def anyChar() = char(_ => true)

  def newLine(): Parser[Seq[Char]] = stringParser(System.lineSeparator())

  def stringParser(string: String): Parser[Seq[Char]] = {
    val charParsingResult: Seq[Parser[Char]] = string.map(charParser)
    val result: Parser[Seq[Char]] = ParserExtensions.sequence(charParsingResult) //TODO: sequence as extension
    result
  }

}
