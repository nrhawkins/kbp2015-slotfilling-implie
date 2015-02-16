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
    
    # If all one case, then modify...
    if term.islower() or term.isupper():
        tokens = term.split()
        newtokens = map(lambda tok: tok if '.' in tok else tok[:1].upper() + tok[1:].lower(), tokens)
        outfile.write(' '.join(newtokens) + '\n')

    # otherwise just write as is.
    else:
        outfile.write(term + '\n')

outfile.close()
