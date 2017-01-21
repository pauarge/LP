# LP - Scripting

Pau Argelaguet i Franquelo - FIB fall 2016

## Project structure

* /
	* `cerca.py` --> Main script
	* `test.sh` --> Test script with various parameters
	* `out/` --> Output files from the script

## How to run

Just `python cerca.py [--key] [--date]` (as specified on the requirements). `.html` files will be generated on the `out/` folder, having the timestamp of the generation as the filename. 

We are assuming that `python` points to a Python 3+ installation. An Internet connection is required to download XML files from the web. 
