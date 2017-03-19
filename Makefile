all:
	corebuild -cflag -g -byte-plugin -use-menhir -tag thread -byte-plugin  -use-ocamlfind -pkgs core,llvm,llvm.analysis Main.native 

clean:
	corebuild -clean
