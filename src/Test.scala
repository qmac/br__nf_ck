import java.io.FileReader
import java.io.FileInputStream
 
object BFTest {
    def main(args: Array[String]): Unit = {
        val reader = new FileReader("TestPrint1.b_f")
        println(BFParser.parseFile(reader))
        // println(BFParser.parseLine("   \t\n"))
    }
}