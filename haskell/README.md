# LP - Lego

Pau Argelaguet i Franquelo - FIB tardor 2016

## Project structure

* /
	* `clean.sh`
	* `compile.sh`
	* `lego.g` --> Source code
	* `README.md` --> This file
	* `test.sh`
	* `bin/`--> Compiled binaries
	* `testcases/` --> Text files with test cases.

## How to run

To run the program, we have three scripts (`clean.sh`, `compile.sh`, `test.sh`). They must be granted execution rights (`chmod +x file`).

### Clean

Removes all old binary files stored in the folder `bin/`.

### Compile

Does all three compilation steps at one. The final executable will be `bin/lego`.

This commands are assuming an standard installation of PCCTS on Ubuntu 16.04. In order to make it work somewhere else, the included directory flag on `g++` command may change. 

### Test

This command executes the two previous ones and then tests the binary functionality against some test cases, located at testcases/. The form of this test cases is an input file `*.inp` and an output file `*.out`.

When tests are run, a third `.tmp`textfile is generated from the output of the program, and then is compared to `.out` using the `diff`command. **Therefore, the tests pass if there's no output on the console while running them.**

## Program output

Here is what the `lego` binary prints to the STDOUT, in the following order. 

### Printing the tree

By default, this option is hidden. In order to enable it, the `main` function of the `lego.g` file must be modified *-uncomment ASTPrint(root)-*. It must look like this:

~~~~
int main() {
    root = NULL;
    ANTLR(lego(&root), stdin);
    ASTPrint(root);
    executeListInstructions(root);
    printBlocks();
    printGrid();
}
~~~~

Then, simply run `compile.sh`.

**Tests will fail when printing the tree, since the output is different.**

### Command output

Some commands print when executed (namely height and error messages when an operation is not allowed).

### Blocks

A list of all the existing blocks on the grid with their data. 

In an unnamed block is inserted, it will appear in this list as `Un`(where n is a natural number).

### Grid

Grid representing the numeric height of each cell. By default all numbers are outputted with length 2 and filling the blanks with '0'. If another precision is desired, it can be modified on the `printGrid` function of the `lego.g` file.

~~~~
cout << setfill('0') << setw(DESIRED WIDTH) << g.height[i][j] << " ";
~~~~

**Tests won't pass if width is changed.**