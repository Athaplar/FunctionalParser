import functionalParsers.Combinator.{charParser, digit}
import functionalParsers.Parser
import functionalParsers.Parser.{Failure, Input, Result, Success}
import functionalParsers.ParserExtensions._
import org.scalatest.FlatSpec

class CombinatorSpec extends FlatSpec {

  "A single character parsing" should "work" in {

    val result: Result[Char] = charParser('c')(Input("cd"))

    result match {
      case Success(current, remainder) =>
        assert(current == 'c')
        assert(remainder == Input("cd", 1))

    }
  }

  "A single character parsing" should "result in Failure" in {

    val result: Result[Char] = charParser('c')(Input("d"))

    result match {
      case Success(current, remainder) =>
        fail()
      case Failure(_) => println("Failed as expected")
    }
  }

  "map combinator on functionalParsers.Parser" should "work" in {

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

  "many extension" should "succeeds" in {

    val result: Result[Seq[Char]] =
      charParser('a').many()(Input("aab"))

    result match {
      case Success(current, remainder) =>
        println(s"$current $remainder")
        assert(current == Seq('a', 'a'))
        assert(remainder == Input("aab", 2))
      case Failure(reason) => fail(reason)
    }
  }

  "many extension1" should "succeed" in {

    val result: Result[Seq[Char]] =
      charParser('a').many()(Input("bbb"))

    result match {
      case Success(current, remainder) =>
        println(s"$current $remainder")
        assert(current == Seq())
        assert(remainder == Input("bbb", 0))
      case Failure(reason) => fail(reason)
    }
  }

  "for comprehension with many" should "succeed" in {

    val parser = for {
      a <- charParser('a')
      b <- charParser('b').many()
      c <- charParser('c')

    } yield (a, b, c)

    println(parser(Input("abbbbbbbc")))
  }

  "atleastOnce" should "work" in {

    val parser1: Parser[Seq[Char]] = charParser('a').atLeastOnce()
    val parser2 = for {
      as <- parser1
      bs <- charParser('b').atLeastOnce()
    } yield (as, bs)

    println(parser1(Input("aab")))
    println(parser1(Input("bbb")))
    println("------------")
    println(parser2(Input("aaabbbb")))
    println(parser2(Input("aaa")))
  }

  "test bed" should "" in {

    val list = 2 :: 3 :: Nil

    println(list)

  }

}
