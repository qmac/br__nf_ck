import scala.util.parsing.combinator._
import java.lang.Integer

object BFParser extends JavaTokenParsers {
    def binary2Decimal(binary: List[Char]) : Int = {
        val sign : Int = if (binary.head == '0') 1 else -1
        val numberString : String = binary.tail.mkString
        
        return Integer.parseInt(numberString, 2) * sign
    }

    def parseFile(in: java.io.Reader) = parseAll(prog, in).get
    def parseLine(line: String) = parseAll(prog, line).get

    override val whiteSpace = """([^(\s\\/_><+-.,\[\])]*)+""".r

    def giveaway: Parser[Any] = "\\"
    def takeaway: Parser[Any] = "/"
    def crossover: Parser[Any] = "_"

    def incrementByte: Parser[Any] = "+"
    def decrementByte: Parser[Any] = "-"
    def incrementPtr: Parser[Any] = ">"
    def decrementPtr: Parser[Any] = "<"
    def output: Parser[Any] = "."
    def input: Parser[Any] = ","
    def forward: Parser[Any] = "["
    def back: Parser[Any] = "]"
    def bfStatement: Parser[Any] = incrementByte | decrementByte | incrementPtr | decrementPtr | output | input | forward | back | giveaway | takeaway

    def tab: Parser[Any] = "\t"
    def space: Parser[Any] = " "
    def lf: Parser[Any] = "\n"
    def wsStatement: Parser[Any] = stack | math | heap | flow | io | giveaway | takeaway
    
    def wsBf = crossover ~ rep(wsStatement) ~ crossover ~ rep(bfStatement)
    def bfWs = crossover ~ rep(bfStatement) ~ crossover ~ rep(wsStatement)
    def prog = rep(bfStatement) ~ crossover ~ rep(wsStatement) ~ rep(bfWs) | 
            rep(bfStatement) ~ rep(wsBf) | 
            rep(bfStatement)
    
    
    def stack    : Parser[Any] = push | duplicate | swap | discard
    def math     : Parser[Any] = wsadd | wssub | wsmulti | wsdiv | wsmod
    def heap     : Parser[Any] = heapstore | heapretrv
    def flow     : Parser[Any] = marklabel | callsubrt | jump | jumpzero | jumpneg | endsubrt | end
    def io       : Parser[Any] = outputchr | outputnum | readchar | readnum

    def digit    : Parser[Char] = (space | tab)                      ^^ {case " " => '0' 
                                                                        case "\t" => '1'}
    def number   : Parser[List[Char]] = (digit).+ <~ lf
    
    def push     : Parser[Any] = space ~ space ~ number             ^^ {case a ~ b ~ n => "push(" + binary2Decimal(n) + ")"}
    def duplicate: Parser[Any] = space ~ lf ~ space                ^^^ "duplicate"
    def swap     : Parser[Any] = space ~ lf ~ tab                  ^^^ "swap"
    def discard  : Parser[Any] = space ~ lf ~ lf                   ^^^ "discard"
    
    def wsadd    : Parser[Any] = lf ~ space ~ space ~ space        ^^^ "wsadd"
    def wssub    : Parser[Any] = lf ~ space ~ space ~ tab          ^^^ "wssub"
    def wsmulti  : Parser[Any] = lf ~ space ~ space ~ lf           ^^^ "wsmulti"
    def wsdiv    : Parser[Any] = lf ~ space ~ tab ~ space          ^^^ "wsdiv"
    def wsmod    : Parser[Any] = lf ~ space ~ tab ~ tab            ^^^ "wsmod"
    
    def heapstore: Parser[Any] = tab ~ tab ~ space                 ^^^ "heapstore"
    def heapretrv: Parser[Any] = tab ~ tab ~ tab                   ^^^ "heapretrv"
    
    def marklabel: Parser[Any] = lf ~ space ~ space ~ number        ^^ {case a ~ b ~ c ~ n => "mark(" + binary2Decimal(n) + ")"}
    def callsubrt: Parser[Any] = lf ~ space ~ tab ~ number          ^^ {case a ~ b ~ c ~ n => "call(" + binary2Decimal(n) + ")"}
    def jump     : Parser[Any] = lf ~ space ~ lf ~ number           ^^ {case a ~ b ~ c ~ n => "jump(" + binary2Decimal(n) + ")"}
    def jumpzero : Parser[Any] = lf ~ tab ~ space ~ number          ^^ {case a ~ b ~ c ~ n => "jumpzero(" + binary2Decimal(n) + ")"}
    def jumpneg  : Parser[Any] = lf ~ tab ~ tab ~ number            ^^ {case a ~ b ~ c ~ n => "jumpneg(" + binary2Decimal(n) + ")"}
    def endsubrt : Parser[Any] = lf ~ tab ~ lf                     ^^^ "endsubrt"
    def end      : Parser[Any] = lf ~ lf ~ lf                      ^^^ "end"
    
    def outputchr: Parser[Any] = tab ~ lf ~ space ~ space          ^^^ "outchr"
    def outputnum: Parser[Any] = tab ~ lf ~ space ~ tab            ^^^ "outnum"
    def readchar : Parser[Any] = tab ~ lf ~ tab ~ space            ^^^ "readchr"
    def readnum  : Parser[Any] = tab ~ lf ~ tab ~ tab              ^^^ "readnum"
}
