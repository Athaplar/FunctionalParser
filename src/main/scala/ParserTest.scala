import Parser.Input
import Parsers._
object ParserTest extends App {

  println(char('1')(Input("cd")))

  println(char('1').map(_.toInt)(Input("1")))

  println(char('1').flatMap(digit)(Input("11")))

  val nestedFlatMap: Parser[(Char, Char)] =
    char('c').flatMap(x => char('b').map(y => (x, y)))

  println(nestedFlatMap(Input("cb")))

  //for-comprehension

  val forComprehension: Parser[(Char, Char)] = for {
    x <- char('c')
    y <- char('b')
  } yield (x, y)

  println(forComprehension(Input("cb")))
}
