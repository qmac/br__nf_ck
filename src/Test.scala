import java.io.FileReader
import java.io.FileInputStream
 
object BFTest {
  def main(args: Array[String]): Unit = {
    val reader = new FileReader("TestAdd1.ws")
    println(BFParser.parseFile(reader))
  }
}