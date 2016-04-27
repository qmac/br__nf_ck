Scala external DSL for the esoteric programming languages brainfuck and whitespace

PL assignment 5



  Siddharth Kumar
  Eugene Ng
  Quinten McNamara
  
  BR__NF_CK: a crossover language between the brainfuck and whitespace programming languages
  
  For every program there exist two memory representations.
  There is a stack that the programmer can use whitespace language to operate on.
  There is also an array of cells that the programmer can use brainfuck language to operate on.
  These two features exist separately and independently.
  
  These structures and the syntax for these languages are not modified.
  
  Brainfuck: 8 commands: > < + - . , [ ]
  Whitespace: 5 Command Types: Stack Manipulation, Arithmetic, Heap Access, Flow Control, and I/O:
    These commands and their parameters consist of whitespace characters: Space, Tab, and LineFeed.
  
  
  Visit http://compsoc.dur.ac.uk/whitespace/tutorial.html for a Whitespace tutorial.
  Visit https://learnxinyminutes.com/docs/brainfuck/      for a Brainfuck tutorial.
  
  BR__NF_CK introduces three new commands that offer an ability to crossover between the two languages.
  
  Crossover: The crossover command tells the interpreter to switch the interepretation of the code to the other language.
    I am coding in the brainfuck environment and want to switch to whitespace:
    > + + + _[SPACE][SPACE][SPACE][Tab][LF]
    This code increments the data pointer in brainfuck and increments that value in that cell by 3. Then it switches to whitespace and pushes 1 onto the stack. Note how there can be whitespace in the brainfuck code that is not interpereted as whitespace is code.
  The crossover command is done by using the underscore character: _
  
  The two data structures, whitespace stack and brainfuck array were independent. But it might be useful for the programmer to communicate between them.
  
  Steal: The steal command reads the value in the inactive language and writes it to the location in the active language.
    If writing in brainfuck, the steal would take whatever is at the top of the stack and write it in the cell currently pointing to.
    If writing in whitespace, the steal would take whatever is at the cell currently pointed to and pushes it onto the stack.
  The steal command is done by using the backslash character: \
  
  Giveaway: The giveaway command is the opposite of the steal. It puts the current value into the current location in the other language
    If writing in brainfuck, the giveaway would take whatever is at the cell currently pointed to and push it onto the stack.
    If writing in whitespace, the giveaway would take whatever is at the top of the stack and write it in the cell currently pointed to by the data pointer.
  The giveaway command is done by using the forewardslash character: /
