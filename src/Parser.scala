import scala.util.parsing.combinator._

object BFParser extends JavaTokenParsers {
    def parseFile(in: java.io.Reader) = parseAll(prog, in).get
    def parseLine(line: String) = parseAll(prog, line).get

    override val whiteSpace = """([^(\s\\/_><+-.,\[\])]*)+""".r

    def tab: Parser[Any] = "\t"
    def space: Parser[Any] = " "
    def lf: Parser[Any] = "\n"
    def statement: Parser[Any] = tab | space | lf | giveaway
    
    def giveaway : Parser[Any] = "\\"

    def prog = rep(statement)
}
