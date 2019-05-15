package functionalParsers

import functionalParsers.Parser.{Failure, Input, Parser, Success}

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

  implicit class RichParser1[T](val parser: Parser[Seq[T]]) {

    def text(): Parser[String] = parser.map(_.mkString)

  }

  implicit class RichParser2[T](val parser: Parser[T]) {

    def map[B](f: T => B): Parser[B] =
      input =>
        parser(input) match {
          case Success(value: T, remainder: Input) =>
            Success(f(value), remainder)
          case Failure(reason) => Failure(reason)
      }

    def flatMap[B](f: T => Parser[B]): Parser[B] =
      input =>
        parser(input) match {
          case Success(value: T, remainder: Input) => f(value)(remainder)
          case Failure(reason)                     => Failure(reason)
      }

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

    def atLeastOnce(): Parser[Seq[T]] =
      for {
        a <- parser
        b <- many()
      } yield a +: b

    def expect[B](parserB: Parser[B]): Parser[T] = input => {

      val result = parserB(input)

      result match {
        case Success(_, _) => Failure(" except parser was suppose to not pass")
        case Failure(_)    => parser(input)
      }
    }

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
