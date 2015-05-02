
import sys
import re

"""
This script takes in a dirty csv of standard country name
to alternate names or forms of the name where each line
is a country and makes a cleaner version.

Input file format:
  Items are tab or comma separated.

Output file format:
  Standard country name followed by a tab, followed by
  a comma separated list of alternate forms or aliases.

Cleans parentheses, brackets, numbers and quotation marks.
This will likely not remove all bad entries, so please
take a look at the resulting file and clean up anything left.
"""

if len(sys.argv) < 3:
  sys.exit("Usage: python make_mapping_file.py [input_file] [output_file]")

lines = file(sys.argv[1], 'r').read().splitlines()
tokenlist = [re.split('\t|\"|\(|\)|/|,|\[|\]|[0-9]', line) for line in lines]
tokenlist = [[tok.strip() for tok in tokens if tok.strip() != ""] for tokens in tokenlist]

out = file(sys.argv[2], 'w')
for tokens in tokenlist:
  out.write(tokens[0])
  out.write('\t')
  out.write(','.join(tokens[1:]))
  out.write('\n')
out.close()
