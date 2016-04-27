import scala.collection.mutable
import scala.io.StdIn.{readLine,readInt}

object Evaluator {
    
    val tapesize = 100
    var tape:Array[Int] = new Array[Int](tapesize)
    
    
    
    // Data Pointer
    var ptr = 0
    
    def eval(cmdlist:List[Any])
    {
        var actual : List[Any] = cmdlist
        actual.foreach(exec(_))
        
        
    }
    
    def exec(cmd:Any) = cmd match
    {
        case ">" => increment_ptr()
        case "<" => decrement_ptr()
        case "+" => tape(ptr) = tape(ptr) + 1
        case "-" => tape(ptr) = tape(ptr) - 1
        case "." => output(tape(ptr))
        case "," => tape(ptr) = readInt()
        case subprogram : List[Any] => eval(subprogram)
    }
    
    def output(num:Int)
    {
        print(num.toChar)
    }
    
    def increment_ptr() {
        if (ptr != tapesize - 1) {
            ptr = ptr + 1
        }
        else {
            ptr = 0
        }
    }
    
    def decrement_ptr() {
        if (ptr != 0)  {
            ptr = ptr - 1
        }
        else {
            ptr = tapesize
        }
    }
    
    
}

