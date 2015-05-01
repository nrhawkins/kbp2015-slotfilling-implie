
import sys
import re

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
