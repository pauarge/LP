#!/bin/bash

mkdir -p bin/
antlr -gt src/lang.g -o bin/
dlg -ci bin/parser.dlg bin/scan.c -o bin/
g++ -o bin/lang bin/lang.c bin/scan.c bin/err.c -I /usr/include/pccts/ -w -std=c++11
rm bin/err.c bin/lang.c bin/mode.h bin/parser.dlg bin/scan.c bin/tokens.h

ghc src/interpreter.hs -o bin/interpreter
rm src/interpreter.o src/interpreter.hi