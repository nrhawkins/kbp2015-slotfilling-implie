#/usr/bin/python
from unidecode import unidecode
import sys

if len(sys.argv) < 3:
  sys.exit("Usage: python transliterate_to_ascii.py [input file] [output file]")

infile = sys.argv[1]
outfile = sys.argv[2]

cities = file(infile, 'r').read().splitlines()
out = file(outfile, 'w')

#trans = [unidecode(unicode(city, 'utf-8')) for city in cities]
trans = [unidecode(unicode(city, 'iso-8859-1')) for city in cities]

out.write('\n'.join(trans))
out.close() 
