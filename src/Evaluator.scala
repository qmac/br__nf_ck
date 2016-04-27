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
    def subtract = stack.push(stack.pop+stack.pop)
    def multiply = stack.push(stack.pop+stack.pop)
    def divide = stack.push(stack.pop+stack.pop)
    def modulus = stack.push(stack.pop+stack.pop)
    def outchr = print(stack.pop.toChar)
    def outnum = print(stack.pop)
    def readchr = stack.push(readChar.toInt)
    def readnum = stack.push(readInt)
}