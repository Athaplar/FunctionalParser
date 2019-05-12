import Parser._

object Parsers {

  def char(predicate: Char => Boolean): Parser[Char] = new Parser[Char] {

    override def apply(input: Input): Result[Char] =
      if (predicate(input.current)) {
        Success(input.current, input.next)
      } else Failure("Failed")
  }

  def charParser(c: Char) = char(i => i == c)

  def digit(c: Char) = char(x => x.isDigit)

}
