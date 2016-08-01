Description of Files:
---------------------

  - util.ml
      Defines useful HashSet module as well as functions that manipulate lists.
  
  - ast.ml    
      Defines the datatypes for the abstract syntax tree (AST) for
      the source language.

  - state.ml
      For representing environments. Used in the translation to lifted-lambda expressions.

  - lexer.mll
  - parser.mly
      A lexer and parser for the source language.

  - machine.ml
      Defines the instruction set of the stack machine and values on the stack.
  
  - pprint.ml
      A pretty printer for the source and target languages.

  - eval.ml
      The interpreter for the source and target languages. A stack-machine
      program can be run both forwards and backwards.
  
  - compiler.ml
      Translates an expression in source language to stack machine instructions.    

  - main.ml
      The top level code that parses in an input file and runs the interpreter.
  
How to compile:
---------------

  If you have make installed, you can simply type "make" at the
  command line. 
