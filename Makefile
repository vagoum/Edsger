all:
	corebuild -cflag -g -byte-plugin -use-menhir -menhir "menhir --explain"  -tag thread -byte-plugin  -use-ocamlfind -pkgs core,llvm,llvm.analysis,ppx_deriving.std Main.native 

clean:
	corebuild -clean
