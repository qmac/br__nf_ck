import java.io.FileReader
import java.io.FileInputStream
 
object BFTest {
    def main(args: Array[String]): Unit = {
        val reader = new FileReader(args(0))
        val parser = BFParser
        val ast = parser.parseAll(parser.prog, reader) match {
            case parser.Success(result, _) => result
            case parser.NoSuccess(msg, _) => throw new RuntimeException("Parsing Failed:" + msg)
        }
        // println(ast)
        BFEvaluator.evaluate(ast)
    }
}