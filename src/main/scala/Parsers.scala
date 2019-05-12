import Parser._

import scala.collection.mutable.ListBuffer

object Parsers {

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

  def many(parser: Parser[Char]): Parser[Seq[Char]] =
    input => {

      var list = ListBuffer[Char]()
      var remainingInput = input
      var flag = true
      while (flag) {

        parser(remainingInput) match {
          case Success(current, remainder) =>
            list = list :+ current
            remainingInput = remainder
          case Failure(_) => flag = false
        }
      }
      Success(list.toList, remainingInput)
    }

  def atLeastOnce(parser: Parser[Char]): Parser[Seq[Char]] =
    for {
      a <- parser
      b <- many(parser)
    } yield a +: b

  /*def once(parser: Parser[Char]): Parser[Char] =
    input => {

      val result: Result[Char] = parser(input)
      if (result.hasFailed) {}

      ???
    }*/

}
