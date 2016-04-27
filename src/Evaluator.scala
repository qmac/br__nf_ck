import scala.collection.mutable.Stack
import scala.io.StdIn._

object BFEvaluator {
    var stack = Stack[Int]()
    
    val tapesize = 100
    var tape:Array[Int] = new Array[Int](tapesize)
    var ptr = 0

    def evaluate(operations: List[Operation]) : Unit = {
        operations match {
            case command :: remainingCommands =>
                execute(command)
                evaluate(remainingCommands)
            case Nil => ;
        }
    }

    def execute(command: Operation) : Unit = {
        command match {
            case WsPush(n) => push(n)
            case WsDuplicate() => duplicate
            case WsSwap() => swap
            case WsDiscard() => discard
            case WsAdd() => add
            case WsSub() => subtract
            case WsMulti() => multiply
            case WsDiv() => divide
            case WsMod() => modulus
            case WsOutChr() => outchr
            case WsOutNum() => outnum
            case WsReadChr() => readchr
            case WsReadNum() => readnum
            case WsEnd() => end
            
            case BfInc() => increment_ptr()
            case BfDec() => decrement_ptr()
            case BfAdd() => tape(ptr) = tape(ptr) + 1
            case BfSub() => tape(ptr) = tape(ptr) - 1
            case BfOut() => output(tape(ptr))
            case BfIn() => tape(ptr) = readInt()
            // TODO: Control Flow
        }
        
    }

    def push(n: Int) = stack.push(n)
    def duplicate = stack.push(stack.top)
    def swap = {
        val prevTop = stack.pop
        val prevNext = stack.pop
        stack.push(prevTop)
        stack.push(prevNext)
    }
    def discard = stack.pop
    def add = stack.push(stack.pop+stack.pop)
    def subtract = {
        val prevTop = stack.pop
        stack.push(stack.pop-prevTop)
    }
    def multiply = stack.push(stack.pop*stack.pop)
    def divide = {
        val prevTop = stack.pop
        stack.push(stack.pop/prevTop)
    }
    def modulus = {
        val prevTop = stack.pop
        stack.push(stack.pop%prevTop)
    }
    def outchr = print(stack.pop.toChar)
    def outnum = print(stack.pop)
    def readchr = stack.push(readChar.toInt)
    def readnum = stack.push(readInt)

    
    def output(num:Int) = print(num.toChar)
    
    def increment_ptr() = {
        if (ptr != tapesize - 1) {
            ptr = ptr + 1
        }
        else {
            ptr = 0
        }
    }
    
    def decrement_ptr() = {
        if (ptr != 0)
            ptr = ptr - 1
        else
            ptr = tapesize
    }
    
    def end = stack.clear
}
