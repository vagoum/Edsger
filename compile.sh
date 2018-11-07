#/usr/bin/bash

opt=false
en=false
for i in $@; do
	case ${i} in
		-O)
			opt=true;;
		-i)
			en=true;;
		*)
			echo "Use: ./compile.sh -O(for optimize) -i(for cat llvm)"
			exit
		esac
done

./Main.native
if ${opt}; then
llc-6.0 -O3 -filetype=obj llvm_code.ll -o out.o
else
llc-6.0 -filetype=obj llvm_code.ll -o out.o
fi
if ${en} ;then
cat llvm_code.ll
fi
clang++-6.0 -g out.o lib.b -o out
