import scala.util.parsing.combinator._
import java.lang.Integer

abstract class Operation
case class Giveaway() extends Operation
case class Takeaway() extends Operation
case class Crossover() extends Operation
case class BfAdd() extends Operation
case class BfSub() extends Operation
case class BfInc() extends Operation
case class BfDec() extends Operation
case class BfOut() extends Operation
case class BfIn() extends Operation
case class BfForward() extends Operation
case class BfBack() extends Operation
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

    def giveaway: Parser[Operation] = "\\"  ^^^ Giveaway()
    def takeaway: Parser[Operation] = "/"   ^^^ Takeaway()
    def crossover: Parser[Operation] = "_"  ^^^ Crossover()

    def incrementByte: Parser[Operation] = "+" ^^^ BfAdd()
    def decrementByte: Parser[Operation] = "-" ^^^ BfSub()
    def incrementPtr: Parser[Operation] = ">"  ^^^ BfInc()
    def decrementPtr: Parser[Operation] = "<"  ^^^ BfDec()
    def output: Parser[Operation] = "."        ^^^ BfOut()
    def input: Parser[Operation] = ","         ^^^ BfIn()
    def forward: Parser[Operation] = "["       ^^^ BfForward()
    def back: Parser[Operation] = "]"          ^^^ BfBack()
    def bfStatement: Parser[Operation] = incrementByte | decrementByte | incrementPtr | decrementPtr | output | input | forward | back | giveaway | takeaway

    def tab: Parser[String] = "\t"
    def space: Parser[String] = " "
    def lf: Parser[String] = "\n"
    def wsStatement: Parser[Operation] = stack | math | heap | flow | io | giveaway | takeaway
    
    //def wsBf: Parser[List[Operation]] = crossover ~ rep(wsStatement) ~ crossover ~ rep(bfStatement)
    //def bfWs: Parser[List[Operation]] = crossover ~ rep(bfStatement) ~ crossover ~ rep(wsStatement)

    def prog: Parser[List[Operation]] = rep(bfStatement) | rep(wsStatement)
    //def prog: Parser[List[List[Operation]]] = rep(wsStatement) | rep(bfStatement) ~ crossover ~ rep(wsStatement) ~ rep(bfWs) |
     //       rep(bfStatement) ~ rep(wsBf) |
      //      rep(bfStatement)
    
    
    def stack    : Parser[Operation] = push | duplicate | swap | discard
    def math     : Parser[Operation] = wsadd | wssub | wsmulti | wsdiv | wsmod
    def heap     : Parser[Operation] = heapstore | heapretrv
    def flow     : Parser[Operation] = marklabel | callsubrt | jump | jumpzero | jumpneg | endsubrt | end
    def io       : Parser[Operation] = outputchr | outputnum | readchar | readnum

    def digit    : Parser[Char] = (space | tab)                      ^^ {case " " => '0'
                                                                        case "\t" => '1'}
    def number   : Parser[List[Char]] = (digit).+ <~ lf
    
    def push     : Parser[Operation] = space ~ space ~ number             ^^ {case a ~ b ~ n => Push(binary2Decimal(n))}
    def duplicate: Parser[Operation] = space ~ lf ~ space                ^^^ Duplicate()
    def swap     : Parser[Operation] = space ~ lf ~ tab                  ^^^ Swap()
    def discard  : Parser[Operation] = space ~ lf ~ lf                   ^^^ Discard()
    
    def wsadd    : Parser[Operation] = tab ~ space ~ space ~ space       ^^^ Wsadd()
    def wssub    : Parser[Operation] = tab ~ space ~ space ~ tab         ^^^ Wssub()
    def wsmulti  : Parser[Operation] = tab ~ space ~ space ~ lf          ^^^ Wsmulti()
    def wsdiv    : Parser[Operation] = tab ~ space ~ tab ~ space         ^^^ Wsdiv()
    def wsmod    : Parser[Operation] = tab ~ space ~ tab ~ tab           ^^^ Wsmod()

    def heapstore: Parser[Operation] = tab ~ tab ~ space                 ^^^ Heapstore()
    def heapretrv: Parser[Operation] = tab ~ tab ~ tab                   ^^^ Heapretrv()
    
    def marklabel: Parser[Operation] = lf ~ space ~ space ~ number        ^^ {case a ~ b ~ c ~ n => Marklabel(binary2Decimal(n))}
    def callsubrt: Parser[Operation] = lf ~ space ~ tab ~ number          ^^ {case a ~ b ~ c ~ n => Callsubrt(binary2Decimal(n))}
    def jump     : Parser[Operation] = lf ~ space ~ lf ~ number           ^^ {case a ~ b ~ c ~ n => Jump(binary2Decimal(n))}
    def jumpzero : Parser[Operation] = lf ~ tab ~ space ~ number          ^^ {case a ~ b ~ c ~ n => Jumpzero(binary2Decimal(n))}
    def jumpneg  : Parser[Operation] = lf ~ tab ~ tab ~ number            ^^ {case a ~ b ~ c ~ n => Jumpneg(binary2Decimal(n))}
    def endsubrt : Parser[Operation] = lf ~ tab ~ lf                     ^^^ Endsubrt()
    def end      : Parser[Operation] = lf ~ lf ~ lf                      ^^^ End()
    
    def outputchr: Parser[Operation] = tab ~ lf ~ space ~ space          ^^^ Outchr()
    def outputnum: Parser[Operation] = tab ~ lf ~ space ~ tab            ^^^ Outnum()
    def readchar : Parser[Operation] = tab ~ lf ~ tab ~ space            ^^^ Readchr()
    def readnum  : Parser[Operation] = tab ~ lf ~ tab ~ tab              ^^^ Readnum()
}