#!/bin/bash

if [ $# -eq 0 ]; then
	echo "No input file specified"
elif [ $# -eq 1 ]; then
	INP=$(readlink -f "$1")
	bin/reader < "$INP" > programhs.txt
	bin/program
	rm programhs.txt
else 
	echo "Too much arguments"
fi