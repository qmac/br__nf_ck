import java.io.FileReader
import java.io.FileInputStream


 
object BFTest {
    def main(args: Array[String]): Unit = {
        // val reader = new FileReader("../../test/TestPrint1.b_f")
        // println(BFParser.parseFile(reader))
        
        var ls : List[Any] = BFParser.parseLine("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>,<.>.").productIterator.toList
        println(Evaluator.eval(ls))
    }
}