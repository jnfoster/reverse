Description of Files:
---------------------
  - ast.ml    
      Defines the datatypes for the abstract syntax tree (AST) for
      the source language.

  - eval.ml
      The interpreter for the source and target languages. A stack-machine
      program can be run both forwards and backwards.

  - lexer.mll
  - parser.mly
      A lexer and parser for the source language.

  - pprint.ml
      A pretty printer for the source and target languages.

  - main.ml
      The top level code that parses in an input file and runs the
      interpreter.

  - machine.ml
      Defines the instruction set of the stack machine and 
      values on the stack.

  - translation.ml
      Translates an expression in source language to stack machine instructions.    
  
How to compile:
---------------

  If you have make installed, you can simply type "make" at the
  command line. 
