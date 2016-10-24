#!/bin/bash

./clean.sh
./compile.sh

bin/lego < testcases/complex.inp > testcases/complex.tmp
diff testcases/complex.out testcases/complex.tmp
rm testcases/complex.tmp

bin/lego < testcases/errors.inp > testcases/errors.tmp
diff testcases/errors.out testcases/errors.tmp
rm testcases/errors.tmp

bin/lego < testcases/pops.inp > testcases/pops.tmp
diff testcases/pops.out testcases/pops.tmp
rm testcases/pops.tmp

bin/lego < testcases/sample.inp > testcases/sample.tmp
diff testcases/sample.out testcases/sample.tmp
rm testcases/sample.tmp
