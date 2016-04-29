all: 
	corebuild -use-menhir -tag thread -use-ocamlfind  -pkg core main.native  
Lexer: Lexer.ml
	ocamlc lexer.ml -o Lexer

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

TESTS := tests/*.eds

.PHONY: tests

tests: Lexer
	@(for test in $(TESTS); do bash run-tests.sh $$test || exit 1; done)

clean:
	rm *.cmi *.cmo lexer.ml Lexer tests/*.out
