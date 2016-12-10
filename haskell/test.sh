#!/bin/bash

if [ $# -eq 0 ]; then
	echo "No input file specified"
elif [ $# -eq 1 ]; then
	bin/lang < $1 > bin/programhs.txt
	bin/interpreter
else 
	echo "Too much arguments"
fi