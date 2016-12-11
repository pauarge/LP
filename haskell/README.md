# LP - Programes imperatius

Pau Argelaguet i Franquelo - FIB tardor 2016

## Project structure

* /
	* `clean.sh` --> Removes old binaries.
	* `compile.sh` --> Compiles all project's code.
	* `README.md` --> This file.
	* `test.sh` --> Runs the binaries against test files.
	* `bin/` --> Compiled binaries.
	* `src/` --> Haskell/PCCTS source files.
	* `testcases/` --> Text files with test cases.

## How to run

To run the interpreter, we have three scripts (`clean.sh`, `compile.sh` and `test.sh`). They mus be granted execution rights (`chmod +x file`).

### Clean

Removes all old binary files stored in the folder `bin/`.

### Compile

Does all compilation steps in one and generates two executables:

* **lang** This is the PCCTS parser. It receives some input through stdin and outputs a string that can be interpreted as a program.
* **interpreter** Reads a file in the same directory called `programhs.txt` and if it contains a valid program, it runs it.

This commands are assuming an standard installation of PCCTS 1.3+ and Haskell (GHC) 7+ on Ubuntu 16.04. In order to make it work somewhere else, the *included directory* flag on `g++` command may change. 

Since random numbers are generated on the Haskell program, GHC's System.Random library must also be installed on the system. Note that this doesn't come as standard on all distributions.

### Test

This file automates the testing process of the whole project. To execute it, the whole program must be compiled (hence, `./compile.sh` must be executed before this script).

It takes a path to a file as a parameter and executes the program contained on that file (parsing it, generating the intermediate programhs.txt and redirecting it through the interpreter). The output of the interpreter is displayed on the stdout.

#### Examples

`./test.sh testcases/sample0.inp`

`./test.sh testcases/sample1.inp # infinte loop`

## Program input

The Haskell program expects the following input:

* `O`/`1` for using Integers or Reals
* `0`/`1`/`2` for the execution type
* In a Manual execution, it expects a list of valid values on the Haskell format (`[1,2,3,4]`)
* In a Multiple tests execution, it expects the number of executions (a natural number).
