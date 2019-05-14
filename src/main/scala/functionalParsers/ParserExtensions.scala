package functionalParsers

import functionalParsers.Parser.{Failure, Success}

import scala.collection.mutable.ListBuffer

object ParserExtensions {

  def sequence[T](parsers: Seq[Parser[T]]): Parser[Seq[T]] = {

    val result: Parser[Seq[T]] =
      parsers.foldLeft(point(Seq[T]()))((agg, curr) => {
        val aggResult: Parser[Seq[T]] = agg.flatMap(x => curr.map(y => x :+ y))
        aggResult
      })

    result
  }

  def point[T](t: T): Parser[T] = input => Success(t, input)

  def except()

  implicit class RichParser[T](val parser: Parser[T]) {

    def or(next: Parser[T]): Parser[T] = input => {
      parser(input) match {
        case s @ Success(_, _) => s
        case Failure(_)        => next(input)
      }
    }

    def end(): Parser[T] = input => {
      parser(input) match {
        case s @ Success(_, remainder) =>
          if (remainder.isEnd) s else Failure("Text did not end!")
        case Failure(reason) => Failure(s"Parsing error s$reason")
      }

    }
    /*
    def atMostOnce(): Parser[T] = input => {
      parser(input) match {
        case s @ Success(_, _) => s
        case Failure(_)        => Success(input.source(input.position), input)
      }
    }*/

    def atLeastOnce(): Parser[Seq[T]] =
      for {
        a <- parser
        b <- many()
      } yield a +: b

    def except(c: Char) = Combinator.except(c)

    def many(): Parser[Seq[T]] =
      input => {

        var list = ListBuffer[T]()
        var remainingInput = input
        var flag = true
        while (flag && !remainingInput.isEnd) {

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
