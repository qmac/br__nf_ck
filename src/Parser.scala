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
case class WsPush(n: Int) extends Operation
case class WsDuplicate() extends Operation
case class WsSwap() extends Operation
case class WsDiscard() extends Operation
case class WsAdd() extends Operation
case class WsSub() extends Operation
case class WsMulti() extends Operation
case class WsDiv() extends Operation
case class WsMod() extends Operation
case class HeapStore() extends Operation
case class HeapRetrv() extends Operation
case class MarkLabel(n: Int) extends Operation
case class CallSubrt(n: Int) extends Operation
case class Jump(n: Int) extends Operation
case class JumpZero(n: Int) extends Operation
case class JumpNeg(n: Int) extends Operation
case class EndSubrt() extends Operation
case class End() extends Operation
case class OutChr() extends Operation
case class OutNum() extends Operation
case class ReadChr() extends Operation
case class ReadNum() extends Operation

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

    def tab = "\t"
    def space = " "
    def lf = "\n"
    def wsStatement: Parser[Operation] = stack | math | heap | flow | io | giveaway | takeaway
    
    //def wsBf: Parser[List[Operation]] = crossover ~ rep(wsStatement) ~ crossover ~ rep(bfStatement)
    //def bfWs: Parser[List[Operation]] = crossover ~ rep(bfStatement) ~ crossover ~ rep(wsStatement)

    def prog: Parser[List[Operation]] = rep(wsStatement) | rep(bfStatement)
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
    
    def wsadd    : Parser[Operation] = tab ~ space ~ space ~ space       ^^^ WsAdd()
    def wssub    : Parser[Operation] = tab ~ space ~ space ~ tab         ^^^ WsSub()
    def wsmulti  : Parser[Operation] = tab ~ space ~ space ~ lf          ^^^ WsMulti()
    def wsdiv    : Parser[Operation] = tab ~ space ~ tab ~ space         ^^^ WsDiv()
    def wsmod    : Parser[Operation] = tab ~ space ~ tab ~ tab           ^^^ WsMod()

    def heapstore: Parser[Operation] = tab ~ tab ~ space                 ^^^ HeapStore()
    def heapretrv: Parser[Operation] = tab ~ tab ~ tab                   ^^^ HeapRetrv()
    
    def marklabel: Parser[Operation] = lf ~ space ~ space ~ number        ^^ {case a ~ b ~ c ~ n => MarkLabel(binary2Decimal(n))}
    def callsubrt: Parser[Operation] = lf ~ space ~ tab ~ number          ^^ {case a ~ b ~ c ~ n => CallSubrt(binary2Decimal(n))}
    def jump     : Parser[Operation] = lf ~ space ~ lf ~ number           ^^ {case a ~ b ~ c ~ n => Jump(binary2Decimal(n))}
    def jumpzero : Parser[Operation] = lf ~ tab ~ space ~ number          ^^ {case a ~ b ~ c ~ n => JumpZero(binary2Decimal(n))}
    def jumpneg  : Parser[Operation] = lf ~ tab ~ tab ~ number            ^^ {case a ~ b ~ c ~ n => JumpNeg(binary2Decimal(n))}
    def endsubrt : Parser[Operation] = lf ~ tab ~ lf                     ^^^ EndSubrt()
    def end      : Parser[Operation] = lf ~ lf ~ lf                      ^^^ End()
    
    def outputchr: Parser[Operation] = tab ~ lf ~ space ~ space          ^^^ OutChr()
    def outputnum: Parser[Operation] = tab ~ lf ~ space ~ tab            ^^^ OutNum()
    def readchar : Parser[Operation] = tab ~ lf ~ tab ~ space            ^^^ ReadChr()
    def readnum  : Parser[Operation] = tab ~ lf ~ tab ~ tab              ^^^ ReadNum()
}