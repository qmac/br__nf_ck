import scala.collection.mutable.Stack
import scala.io.StdIn._

object BFEvaluator {
    var stack = Stack[Int]()

    def evaluate(operations: Any) : Unit = {
        operations match {
            case command :: remainingCommands => execute(command)
            case Nil => ;
        }
    }

    def execute(command: Any) : Unit = {
        command match {
            case "push" => push(10)
            case "duplicate" => duplicate
            case "swap" => swap
            case "discard" => discard
            case "wsadd" => add
            case "wssub" => subtract
            case "wsmulti" => multiply
            case "wsdiv" => divide
            case "wsmod" => modulus
            case "outchr" => outchr
            case "outnum" => outnum
            case "readchr" => readchr
            case "readnum" => readnum
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
}