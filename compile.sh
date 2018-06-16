#/usr/bin/bash
./Main.native 
llc-6.0 -filetype=obj a2.ll -o a.o
clang++-6.0 -g a.o lib.b -o a
