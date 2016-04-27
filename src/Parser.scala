import scala.util.parsing.combinator._
import java.lang.Integer

abstract class Operation
case class Push(n: Int) extends Operation
case class Duplicate() extends Operation
case class Swap() extends Operation
case class Discard() extends Operation
case class Wsadd() extends Operation
case class Wssub() extends Operation
case class Wsmulti() extends Operation
case class Wsdiv() extends Operation
case class Wsmod() extends Operation
case class Heapstore() extends Operation
case class Heapretrv() extends Operation
case class Marklabel(n: Int) extends Operation
case class Callsubrt(n: Int) extends Operation
case class Jump(n: Int) extends Operation
case class Jumpzero(n: Int) extends Operation
case class Jumpneg(n: Int) extends Operation
case class Endsubrt() extends Operation
case class End() extends Operation
case class Outchr() extends Operation
case class Outnum() extends Operation
case class Readchr() extends Operation
case class Readnum() extends Operation

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
    def prog = rep(wsStatement) | rep(bfStatement) ~ crossover ~ rep(wsStatement) ~ rep(bfWs) | 
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
    
    def push     : Parser[Any] = space ~ space ~ number             ^^ {case a ~ b ~ n => Push(binary2Decimal(n))}
    def duplicate: Parser[Any] = space ~ lf ~ space                ^^^ Duplicate()
    def swap     : Parser[Any] = space ~ lf ~ tab                  ^^^ Swap()
    def discard  : Parser[Any] = space ~ lf ~ lf                   ^^^ Discard()
    
    def wsadd    : Parser[Any] = tab ~ space ~ space ~ space        ^^^ Wsadd()
    def wssub    : Parser[Any] = tab ~ space ~ space ~ tab          ^^^ Wssub()
    def wsmulti  : Parser[Any] = tab ~ space ~ space ~ lf           ^^^ Wsmulti()
    def wsdiv    : Parser[Any] = tab ~ space ~ tab ~ space          ^^^ Wsdiv()
    def wsmod    : Parser[Any] = tab ~ space ~ tab ~ tab            ^^^ Wsmod()
    
    def heapstore: Parser[Any] = tab ~ tab ~ space                 ^^^ Heapstore()
    def heapretrv: Parser[Any] = tab ~ tab ~ tab                   ^^^ Heapretrv()
    
    def marklabel: Parser[Any] = lf ~ space ~ space ~ number        ^^ {case a ~ b ~ c ~ n => Marklabel(binary2Decimal(n))}
    def callsubrt: Parser[Any] = lf ~ space ~ tab ~ number          ^^ {case a ~ b ~ c ~ n => Callsubrt(binary2Decimal(n))}
    def jump     : Parser[Any] = lf ~ space ~ lf ~ number           ^^ {case a ~ b ~ c ~ n => Jump(binary2Decimal(n))}
    def jumpzero : Parser[Any] = lf ~ tab ~ space ~ number          ^^ {case a ~ b ~ c ~ n => Jumpzero(binary2Decimal(n))}
    def jumpneg  : Parser[Any] = lf ~ tab ~ tab ~ number            ^^ {case a ~ b ~ c ~ n => Jumpneg(binary2Decimal(n))}
    def endsubrt : Parser[Any] = lf ~ tab ~ lf                     ^^^ Endsubrt()
    def end      : Parser[Any] = lf ~ lf ~ lf                      ^^^ End()
    
    def outputchr: Parser[Any] = tab ~ lf ~ space ~ space          ^^^ Outchr()
    def outputnum: Parser[Any] = tab ~ lf ~ space ~ tab            ^^^ Outnum()
    def readchar : Parser[Any] = tab ~ lf ~ tab ~ space            ^^^ Readchr()
    def readnum  : Parser[Any] = tab ~ lf ~ tab ~ tab              ^^^ Readnum()
}