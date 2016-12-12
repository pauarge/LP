#!/bin/bash

mkdir -p bin/
antlr -gt src/reader.g -o bin/
dlg -ci bin/parser.dlg bin/scan.c -o bin/
g++ -o bin/reader bin/reader.c bin/scan.c bin/err.c -I /usr/include/pccts/ -w -std=c++11
rm bin/err.c bin/reader.c bin/mode.h bin/parser.dlg bin/scan.c bin/tokens.h

ghc src/program.hs -o bin/program
rm src/program.o src/program.hi