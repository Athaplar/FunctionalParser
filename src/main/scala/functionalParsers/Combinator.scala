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

  def charParser(c: Char) = char(i => i == c)

  def digit(c: Char) = char(x => x.isDigit)

}
