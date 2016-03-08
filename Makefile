Lexer: Lexer.ml
	ocamlc lexer.ml -o Lexer


Lexer.ml: Lexer.mll
	ocamllex Lexer.mll


clean:
	rm *.cmi *.cmo lexer.ml Lexer



