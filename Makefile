all:
	corebuild -use-menhir -tag thread -use-ocamlfind -pkg core Main.native

clean:
	corebuild -clean
