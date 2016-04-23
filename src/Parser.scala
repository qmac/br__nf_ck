import scala.util.parsing.combinator._

object BFParser extends JavaTokenParsers {
    def parseFile(in: java.io.Reader) = parseAll(prog, in).get
    def parseLine(line: String) = parseAll(prog, line).get

    override val whiteSpace = """([^(\s\\/_><+-.,\[\])]*)+""".r

    def tab: Parser[Any] = "\t" ^^^ "[Tab]"
    def space: Parser[Any] = " " ^^^ "[Space]"
    def lf: Parser[Any] = "\n" ^^^ "[LF]"
    def statement: Parser[Any] = tab | space | lf | giveaway
    
    def giveaway: Parser[Any] = "\\"
    def takeaway: Parser[Any] = "/"
    def changeover: Parser[Any] = "_"

    def incrementByte: Parser[Any] = "+"
    def decrementByte: Parser[Any] = "-"
    def incrementPtr: Parser[Any] = ">"
    def decrementPtr: Parser[Any] = "<"
    def output: Parser[Any] = "."
    def input: Parser[Any] = ","
    def forward: Parser[Any] = "["
    def back: Parser[Any] = "]"

    def prog = rep(statement)
}
