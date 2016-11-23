#!/bin/bash

mkdir -p bin/
antlr -gt lang.g -o bin/
dlg -ci bin/parser.dlg bin/scan.c -o bin/
g++ -o bin/lang bin/lang.c bin/scan.c bin/err.c -I /usr/include/pccts/ -w -std=c++11