import scala.util.parsing.combinator._

object BFParser extends JavaTokenParsers {
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


    def tab: Parser[Any] = "\t"
    def space: Parser[Any] = " "
    def lf: Parser[Any] = "\n"
    def statement: Parser[Any] = stack | math | heap | flow | io | number
    

    def prog = rep(statement)
    
    
    def stack    : Parser[Any] = push | duplicate | swap | discard
    def math     : Parser[Any] = wsadd | wssub | wsmulti | wsdiv | wsmod
    def heap     : Parser[Any] = heapstore | heapretrv
    def flow     : Parser[Any] = marklabel | callsubrt | jump | jumpzero | jumpneg | endsubrt | end
    def io       : Parser[Any] = outputchr | outputnum | readchar | readnum

    def digit    : Parser[Any] =  (space | tab)
    def number   : Parser[Any] =  (digit).* ~ lf                  ^^^ "number"
    def push     : Parser[Any] = space ~ space ~  number           ^^^ "push"
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
    
    def marklabel: Parser[Any] = lf ~ space ~ space ~ number ~ lf  ^^^ "mark"
    def callsubrt: Parser[Any] = lf ~ space ~ tab ~ number ~ lf    ^^^ "call"
    def jump     : Parser[Any] = lf ~ space ~ lf ~ number ~ lf     ^^^ "jump"
    def jumpzero : Parser[Any] = lf ~ tab ~ space ~ number ~ lf    ^^^ "jumpzero"
    def jumpneg  : Parser[Any] = lf ~ tab ~ tab ~ number ~ lf      ^^^ "jumpneg"
    def endsubrt : Parser[Any] = lf ~ tab ~ lf                     ^^^ "endsubrt"
    def end      : Parser[Any] = lf ~ lf ~ lf                      ^^^ "end"
    
    def outputchr: Parser[Any] = tab ~ lf ~ space ~ space          ^^^ "outchr"
    def outputnum: Parser[Any] = tab ~ lf ~ space ~ tab            ^^^ "outnum"
    def readchar : Parser[Any] = tab ~ lf ~ tab ~ space            ^^^ "readchr"
    def readnum  : Parser[Any] = tab ~ lf ~ tab ~ tab              ^^^ "readnum"
    
    
    
    
    
}
