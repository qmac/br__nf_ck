import scala.collection.mutable.Stack
import scala.io.StdIn.{readInt,readChar}

object BFEvaluator {
    // Top level data
    var bfmode = true
    var pc = 0

    // BF data
    var brackets = Stack[Int]()
    var pairs = List[(Int, Int)]()
    var brackmap = Map[Int,Int]()

    val tapesize = 100
    var tape: Array[Int] = new Array[Int](tapesize)
    var ptr = 0
    
    // WS data
    var stack = Stack[Int]()
    var heap = Map[Int, Int]()
    var jumpTable = Map[String, Int]()
    var callStack = Stack[Int]()

    def wellform(idx: Int) : Unit = {
        if (brackets.isEmpty) {
            pc = -1
            System.out.println("Syntax error with mismatched []:")
            System.exit(0)
        }
        else {
            var keep : Int = brackets.pop
            pairs = (idx,keep)::pairs
            pairs = (keep,idx)::pairs
        }
    }

    def evaluate(operations: List[Operation]) : Unit = {
        val len: Int = operations.length
        
        var i = 0
        while(i < len) {
            operations(i) match {
                case BfForward() => brackets.push(i)
                case BfBack()    => wellform(i)
                case WsMarkLabel(n) => jumpTable = jumpTable + (n -> i)
                case default => ;
            }
            i += 1
        }
        
        if(!brackets.isEmpty) {
            System.out.println("Syntax Error with mismatched []")
            System.exit(0)
        }
        
        brackmap = pairs.toMap
  
        while( pc >= 0 && pc < len) {
            execute(operations(pc))
            pc += 1
        }
    }

    def execute(command: Operation) : Unit = {
        command match {
            case WsPush(n) => push(n)
            case WsDuplicate() => duplicate
            case WsSwap() => swap
            case WsDiscard() => discard
            case WsAdd() => add
            case WsSub() => subtract
            case WsMulti() => multiply
            case WsDiv() => divide
            case WsMod() => modulus
            case WsOutChr() => outchr
            case WsOutNum() => outnum
            case WsReadChr() => readchr
            case WsReadNum() => readnum
            case WsMarkLabel(n) => ;
            case WsCallSubrt(n) => callSubrt(n)
            case WsJump(n) => jump(n)
            case WsJumpZero(n) => jumpZero(n)
            case WsJumpNeg(n) => jumpNeg(n)
            case WsEndSubrt() => endSubrt
            case WsHeapStore() => heapStore
            case WsHeapRetrv() => heapRetrv
            case WsEnd() => end
            
            //TODO add no-ops for whitespace characters in bf
            
            case Giveaway() => give
            case Takeaway() => take
            case Crossover() => cross
            
            case BfInc() => incrementPtr
            case BfDec() => decrementPtr
            case BfAdd() => incrementVal
            case BfSub() => decrementVal
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
    def readchr = heap = heap + (stack.pop -> readChar)
    def readnum = heap = heap + {stack.pop -> readInt}
    def jump(n: String) = {
        pc = jumpTable.getOrElse(n, -1)
    }
    def jumpZero(n: String) = {
        if(stack.pop == 0) jump(n)
    }
    def jumpNeg(n: String) = {
        if(stack.pop < 0) jump(n)
    }
    def callSubrt(n: String) = {
        callStack.push(pc)
        jump(n)
    }
    def endSubrt = {
        pc = callStack.pop
    }
    def heapStore = {
        val data = stack.pop
        heap = heap + {stack.pop -> data}
    }
    def heapRetrv = {
        val address = stack.pop
        stack.push(heap.getOrElse(address, -1))
    }

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

    def incrementPtr() = {
        if (ptr != tapesize - 1) {
            ptr = ptr + 1
        }
        else {
            ptr = 0
        }
    }
    
    def decrementPtr() = {
        if (ptr != 0)
            ptr = ptr - 1
        else
            ptr = tapesize
    }
    
    def incrementVal() = {
        tape(ptr) = tape(ptr) + 1
    }
    
    def decrementVal()= {
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
