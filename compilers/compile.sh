#!/bin/bash

mkdir -p bin/
antlr -gt lego.g -o bin/
dlg -ci bin/parser.dlg bin/scan.c -o bin/
g++ -o bin/lego bin/lego.c bin/scan.c bin/err.c -I /usr/include/pccts/ -w -std=c++11
