import scala.collection.mutable.Stack
import scala.io.StdIn._

object BFEvaluator {
    
    var bfmode = true
    var pc = 0
    var brackets = Stack[Int]()
    var pairs : List[(Int, Int)] = List[(Int, Int)]()
    
    var brackmap : Map[Int,Int] = Map[Int,Int]()
    
    def wellform(idx: Int) : Unit = {
        if (brackets.isEmpty)
        {
            pc = -1
            System.out.println("Syntax error with mismatched []:")
            System.exit(0)
        }
        else
        {
            var keep : Int = brackets.pop
            pairs = (idx,keep)::pairs
            pairs = (keep,idx)::pairs
        }
    }
    
    var stack = Stack[Int]()
    
    val tapesize = 100
    var tape:Array[Int] = new Array[Int](tapesize)
    var ptr = 0

    def evaluate(operations: List[Operation]) : Unit = {
        
        val len:Int = operations.length
        
        var i = 0
        while( i < len)
        {
            operations(i) match {
                case BfForward() => brackets.push(i)
                case BfBack()    => wellform(i)
                case default => ;
            }
            i += 1
        }
        

        if(!brackets.isEmpty)
        {
            System.out.println("Syntax Error with mismatched []")
            System.exit(0)
        }
        
        brackmap = pairs.toMap
  
        while( pc >= 0 && pc < len)
        {
            execute(operations(pc))
            pc += 1
        }
    }

    def execute(command: Operation) : Unit = {
        command match {
            case Push(n) => push(n)
            case Duplicate() => duplicate
            case Swap() => swap
            case Discard() => discard
            case Wsadd() => add
            case Wssub() => subtract
            case Wsmulti() => multiply
            case Wsdiv() => divide
            case Wsmod() => modulus
            case Outchr() => outchr
            case Outnum() => outnum
            case Readchr() => readchr
            case Readnum() => readnum
            
            case Marklabel(n: Int) => //probably a no-op because done before execute
            case Callsubrt(n: Int) => // change pc to val in jump table and push curr pc to call stack
            case Jump(n: Int) =>      // change pc to val in jump table
            case Jumpzero(n: Int) =>  // similair with check
            case Jumpneg(n: Int) =>   // similair with check
            case Endsubrt()      =>   // change pc to callstack.pop

            case End() => end
            
            //TODO add no-ops for whitespace characters in bf
            
            case Giveaway() => give
            case Takeaway() => take
            case Crossover() => cross
            
            case BfInc() => increment_ptr
            case BfDec() => decrement_ptr
            case BfAdd() => increment_val
            case BfSub() => decrement_val
            case BfOut() => outcell
            case BfIn() => incell
            case BfForward() => fward
            case BfBack() => bward
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
    def add = stack.push(stack.pop+stack.pop)
    def subtract = {
        val prevTop = stack.pop
        stack.push(stack.pop-prevTop)
    }
    def multiply = stack.push(stack.pop*stack.pop)
    def divide = {
        val prevTop = stack.pop
        stack.push(stack.pop/prevTop)
    }
    def modulus = {
        val prevTop = stack.pop
        stack.push(stack.pop%prevTop)
    }
    def outchr =  print(stack.pop.toChar)
    def outnum =  print(stack.pop)
    def readchr = stack.push(readChar.toInt)
    def readnum = stack.push(readInt)
    
    
    def give = {
        if (bfmode)
            push(tape(ptr))
        else
            tape(ptr) = stack.pop
    }
    
    def take = {
        if(bfmode)
            tape(ptr) = stack.pop
        else
            push(tape(ptr))
    }
    
    def cross = {
        bfmode = !bfmode
    }

    def increment_ptr() = {
        if (ptr != tapesize - 1) {
            ptr = ptr + 1
        }
        else {
            ptr = 0
        }
    }
    
    def decrement_ptr() = {
        if (ptr != 0)
            ptr = ptr - 1
        else
            ptr = tapesize
    }
    
    def increment_val() = {
        tape(ptr) = tape(ptr) + 1
    }
    
    def decrement_val()= {
        tape(ptr) = tape(ptr) - 1
    }
    
    def outcell = print(tape(ptr).toChar)
    def incell =  tape(ptr) = readInt()
    def fward = {
        if (tape(ptr) == 0)
            pc = brackmap.getOrElse(pc,0)
    }
    
    def bward = {
        if (tape(ptr) != 0)
            pc = brackmap.getOrElse(pc,0)
    }
    
    def end = {
        stack.clear
        pc = -2
    }
}
