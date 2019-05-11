import Parser.{Failure, Input, Result, Success}
import Parsers.{charParser, digit}
import org.scalatest.FlatSpec

class ParsersSpec extends FlatSpec {

  "A single character parsing" should "work" in {

    val result: Result[Char] = charParser('c')(Input("cd"))

    result match {
      case Success(current, remainder) =>
        assert(current == 'c')
        assert(remainder == Input("cd", 1))

    }
  }

  "map combinator on Parser" should "work" in {

    val result: Result[Int] =
      charParser('1').map(_.toString.toInt)(Input("123"))

    result match {
      case Success(current, remainder) =>
        assert(current == 1)
        assert(remainder == Input("123", 1))
    }
  }

  "Basic parser chaining with flatMap" should "work" in {

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

  "many extension" should "work" in {

    val result: Result[Seq[Char]] =
      Parsers.many(charParser('a'))(Input("aa"))

    result match {
      case Success(current, remainder) =>
        println(s"$current $remainder")
        assert(current == Seq('a', 'a'))
        assert(remainder == Input("aa", 2))
      case Failure(reason) => fail(reason)
    }
  }

  "test bed" should "" in {

    val list = 2 :: 3 :: Nil

    println(list)

  }

}
