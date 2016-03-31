#!/bin/bash
set -e
./Lexer < $1 > $1.out 2>&1 
diff -q  $1 $1.out
