import java.io.FileReader
import java.io.FileInputStream
 
object BFTest {
    def main(args: Array[String]): Unit = {
        val reader = new FileReader("../test/TestPrint1.ws")
        val ast = BFParser.parseFile(reader)
        println(ast)
        BFEvaluator.evaluate(ast)

        // println(BFParser.parseLine("   \t\n"))
    }
}