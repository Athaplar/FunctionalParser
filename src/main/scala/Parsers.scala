import Parser._

object Parsers {


  def char1(predicate: Char => Boolean): Parser[Char] = new Parser[Char] {

    override def apply(input: Input): Result[Char] =
      if (predicate(input.current)) {
        Success(input.current, input.next)
      } else Failure("Failed")
  }

  def char(c: Char) = char1(i => i == c)

  def digit(c: Char) = char1(x => x.isDigit)

}
