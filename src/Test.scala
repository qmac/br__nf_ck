import java.io.FileReader
import java.io.FileInputStream
 
object BFTest {
  def main(args: Array[String]): Unit = {
    val reader = new FileReader("test-source.txt")
    println(BFParser.parseLine("\t \n"))
  }
}