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
      var resultRemainder = input
      var flag = true
      while (flag) {

        parser(resultRemainder) match {
          case Success(current, remainder) =>
            list = list :+ current
            resultRemainder = remainder
          case Failure(_) => flag = false
        }
      }
      if (list.nonEmpty) Success(list.toList, resultRemainder) else Failure("")
    }

}
