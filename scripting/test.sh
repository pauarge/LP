#!/usr/bin/env bash

python3 cerca.py --key '("pintura","musica")'
python3 cerca.py --key '["taller","horta",("musica","pintura")]'
python3 cerca.py --key '"taller"'
python3 cerca.py --date '["15/01/2017",("19/01/2017",-1,1),("25/01/2017",0,1)]'
python3 cerca.py --date '("26/01/2017",-1,1)'
python3 cerca.py --date '"27/01/2017"'

python3 cerca.py --key '("pintura","musica")' --date '["18/01/2017",("22/01/2017",-1,1),("30/01/2017",0,1)]'
python3 cerca.py --key '["taller","horta",("musica","pintura")]' --date '("19/01/2017",-1,1)'
python3 cerca.py --key '"taller"' --date '"20/01/2017"'
