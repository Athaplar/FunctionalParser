package functionalParsers

object Parser {

  type Parser[T] = Input => Result[T]

  case class Input(source: String, position: Int = 0) {

    def isEnd: Boolean = source.size == position

    def next: Input = {
      copy(position = position + 1)
    }

    def current: Char = source(position)
  }

  sealed abstract class Result[T](val hasFailed: Boolean)

  case class Success[T](current: T, remainder: Input)
      extends Result[T](hasFailed = false)

  case class Failure[T](reason: String) extends Result[T](hasFailed = true)

}
