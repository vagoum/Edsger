#/usr/bin/bash
./Main.native 
llc-3.9 -filetype=obj a2.ll -o a.o
clang++ -g a.o lib.b -o a
