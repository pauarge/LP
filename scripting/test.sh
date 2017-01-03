#!/usr/bin/env bash

python3 cerca.py --key '("pintura","musica")'
python3 cerca.py --key '["taller","horta",("musica","pintura")]'
python3 cerca.py --key '"taller"'
python3 cerca.py --date '["03/01/2017",("06/01/2017",-1,1),("14/01/2017",0,1)]'
python3 cerca.py --date '("06/01/2017",-1,1)'
python3 cerca.py --date '"03/01/2017"'

python3 cerca.py --key '("pintura","musica")' --date '["03/01/2017",("06/01/2017",-1,1),("14/01/2017",0,1)]'
python3 cerca.py --key '["taller","horta",("musica","pintura")]' --date '("06/01/2017",-1,1)'
python3 cerca.py --key '"taller"' --date '"03/01/2017"'
