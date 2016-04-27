import scala.collection.mutable.Stack

object BFEvaluator {
	var stack = Stack[Int]()

    def evaluate(operations: Any) : Unit = {
        operations match {
            case command :: remainingCommands => execute(command)
            case Nil => ;
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
    def execute(command: Any) : Unit = {
    	
    }
}