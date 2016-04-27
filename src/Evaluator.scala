import scala.collection.mutable.Stack

object BFEvaluator {
    def evaluate(operations: Any) : Unit = {
        operations match {
            case command :: remainingCommands => execute(command)
            case Nil => ;
        }
    }
    def execute(command: Any) : Unit = {

    }
}