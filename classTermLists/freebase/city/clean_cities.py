#!/usr/bin/python

import sys
import re

if len(sys.argv) < 3:
  sys.exit("Usage: python clean_cities.py [input file] [output file]") 

inputfile = sys.argv[1]
outputfile = sys.argv[2]

lines = file(inputfile, 'r').read().splitlines()
out = file(outputfile, 'w')

# Remove substrings in parentheses.
lines = [re.sub("\(.*\)", "", line) for line in lines]

# Split slash separated cities.
sep = []
for city in lines:
    sep.extend(city.split('/'))

# Remove ones that contain the word 'county'
lines = [term for term in lines if 'county' not in term.lower()]

# Take only the first term for any that are comma separated.
lines = [tokens.split(",")[0] for tokens in sep]

# Fix capitalization if all caps.
capfixed = []
for lines in lines:
    tokens = lines.split()
    if ''.join(tokens).isupper():
        capfixed.append(['{}{}'.format(token[0].upper(), token[1:].lower()) for token in tokens])
    else:
        capfixed.append(tokens)

# Remove duplicates.
cityset = list(set([' '.join(tokens) for tokens in capfixed]))

# trim
cityset = [city.strip() for city in cityset if city is not '']

out.write('\n'.join(cityset))

out.close()
