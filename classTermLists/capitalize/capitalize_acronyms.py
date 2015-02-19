#!usr/bin/python

# First argument is the input file,
# second argument is the output file

import sys
import string

infile = file(sys.argv[1], 'r')
outfile = file(sys.argv[2], 'w')

for term in infile.read().splitlines():
    if term == '':
        continue
    
    # If 3 characters or less, it's probably an acronym... So capitalize.
    if len(term) <= 3:
        outfile.write(term.upper() + '\n')

    # otherwise just write as is.
    else:
        outfile.write(term + '\n')

outfile.close()
