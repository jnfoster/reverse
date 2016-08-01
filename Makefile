MAIN=rev

OBJS = util.cmo ast.cmo state.cmo lifting.cmo lexer.cmo parser.cmo machine.cmo pprint.cmo eval.cmo compiler.cmo main.cmo

%.cmo : %.ml
	ocamlc -c -g $<

%.cmi : %.mli
	ocamlc -c -g $<


$(MAIN): $(OBJS)
	ocamlc -g -o $(MAIN) $(OBJS)

lexer.ml : lexer.mll
	ocamllex -q $<

lexer.cmo : parser.cmi lexer.ml
	ocamlc -c -g lexer.ml

parser.ml : parser.mly
	ocamlyacc -q $<

parser.mli : parser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN)
