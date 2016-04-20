import scala.util.parsing.combinator._

object BFParser extends JavaTokenParsers {
	def parse(in: java.io.Reader) = parseAll(prog, in).get

	def prog: Parser[List[Any]] = rep("")
}
