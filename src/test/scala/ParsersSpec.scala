import Parser.{Input, Result, Success}
import Parsers.{charParser, digit}
import org.scalatest.FlatSpec

class ParsersSpec extends FlatSpec {

  "A single character parsing" should "work" in {

    val result: Result[Char] = charParser('c')(Input("cd"))

    result match {
      case Success(current, remainder) =>
        assert(current == 'c')
        assert(remainder == Input("cd", isEnd = false, 1))

    }
  }

  "map combinator on Parser" should "work" in {

    val result: Result[Int] =
      charParser('1').map(_.toString.toInt)(Input("123"))

    result match {
      case Success(current, remainder) =>
        assert(current == 1)
        assert(remainder == Input("123", isEnd = false, 1))
    }
  }

  "Other combinators" should "work" in {

    println(charParser('1').flatMap(digit)(Input("11")))

    val nestedFlatMap: Parser[(Char, Char)] =
      charParser('c').flatMap(x => charParser('b').map(y => (x, y)))

    println(nestedFlatMap(Input("cb")))

    //for-comprehension

    val forComprehension: Parser[(Char, Char)] = for {
      x <- charParser('c')
      y <- charParser('b')
    } yield (x, y)

    println(forComprehension(Input("cb")))

  }

}
