See Homework 4 for instructions.

Description of Files:
---------------------
  - ast.ml    
      Defines the datatypes for the abstract syntax trees (ASTs) for
      both the source and target languages.

  - eval.ml
      The interpreter for the ASTs. You need to implement an
      interpreter for the target language. You can optionally
      implement an interpreter for the source language if you would
      like.

  - lexer.mll
  - parser.mly
      A lexer and parser for the source and target languages.

  - pprint.ml
      A pretty printer for the ASTs.

  - main.ml
      The top level code that parses in an input file and runs the
      interpreter.

  - examples
      Some test programs in the source language.

How to compile:
---------------

  If you have make installed, you can simply type "make" at the
  command line. Otherwise, if you are on a Windows machine, you can
  edit the make.bat file to point to your installation of Ocaml, and
  execute the make.bat file. If neither of these methods work, you
  should execute the commands listed in the make.bat file.

  Successful compilation will produce an executable file called "lam".


