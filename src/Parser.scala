import scala.util.parsing.combinator._

object BFParser extends JavaTokenParsers {
	def parseFile(in: java.io.Reader) = parseAll(prog, in).get
	def parseLine(line: String) = parseAll(prog, line).get

	def prog: Parser[List[Any]] = rep("")
}

	override val skipWhitespace = false

	def tab: Parser[Any] = "	" ~ statement
	def space: Parser[Any] = " " ~ statement
	def lf: Parser[Any] = "\n"
	def statement: Parser[Any] = tab | space | lf

	def prog = rep(statement)
}
