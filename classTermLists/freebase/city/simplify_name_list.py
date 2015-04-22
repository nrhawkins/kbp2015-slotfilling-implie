# Removes all but the first token in each line and lowercases the names.
# The original lists have rank and probability info in following tokens of each
# line.

import sys

if len(sys.argv) < 3:
  sys.exit("Usage: python simplify_name_list.py [input file] [output file]")

lines = file(sys.argv[1], 'r').read().splitlines()
lines = [line.split()[0].lower() for line in lines]

out = file(sys.argv[2], 'w')

out.write('\n'.join(lines))
out.close()
