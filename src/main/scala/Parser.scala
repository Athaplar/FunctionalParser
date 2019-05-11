import Parser.{Failure, Input, Result, Success}

trait Parser[T] {
  self =>

  def apply(input: Input): Result[T]

  def map[B](f: T => B): Parser[B] =
    i =>
      self.apply(i) match {
        case Success(value: T, remainder: Input) => Success(f(value), remainder)
        case Failure(reason)                     => Failure(reason)
    }

  def flatMap[B](f: T => Parser[B]): Parser[B] =
    i =>
      self.apply(i) match {
        case Success(value: T, remainder: Input) => f(value)(remainder)
        case Failure(reason)                     => Failure(reason)
    }

}

object Parser {

  case class Input(source: String, position: Int = 0) {

    def isEnd: Boolean = source.size == position

    def next: Input = {
      copy(position = position + 1)
    }

    def current: Char = source(position)
  }

  sealed abstract class Result[T]

  case class Success[T](current: T, remainder: Input) extends Result[T]

  case class Failure[T](reason: String) extends Result[T]

}
