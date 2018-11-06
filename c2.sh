#/usr/bin/bash
llc-6.0 -filetype=obj llvm_code.ll -o out.o
clang++-6.0 -g out.o lib.b -o out
