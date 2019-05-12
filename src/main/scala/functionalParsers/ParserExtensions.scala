package functionalParsers

import functionalParsers.Parser.{Failure, Success}

import scala.collection.mutable.ListBuffer

object ParserExtensions {

  implicit class RichParser[T](val parser: Parser[T]) {

    def atLeastOnce(): Parser[Seq[T]] =
      for {
        a <- parser
        b <- many()
      } yield a +: b

    def many(): Parser[Seq[T]] =
      input => {

        var list = ListBuffer[T]()
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

  }

}
