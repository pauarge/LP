#!/bin/bash

if [ $# -eq 0 ]; then
	echo "No input file specified"
elif [ $# -eq 1 ]; then
	INP=$(readlink -f "$1")
	bin/lang < "$INP" > bin/programhs.txt
	bin/interpreter
else 
	echo "Too much arguments"
fi