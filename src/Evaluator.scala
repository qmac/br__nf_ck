import scala.collection.mutable.Stack
import scala.io.StdIn._

object BFEvaluator {
    var stack = Stack[Int]()

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
            case Push(n) => push(n)
            case Duplicate() => duplicate
            case Swap() => swap
            case Discard() => discard
            case Wsadd() => add
            case Wssub() => subtract
            case Wsmulti() => multiply
            case Wsdiv() => divide
            case Wsmod() => modulus
            case Outchr() => outchr
            case Outnum() => outnum
            case Readchr() => readchr
            case Readnum() => readnum
            case End() => end
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
    def end = stack.clear
}