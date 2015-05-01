#!/usr/bin/python

import sys

wd = "/projects/WebWare6/DEFT_corpora/TAC_2010/TAC_2010_KBP_Source_Data/data/2009/nw"

r1000 = file(wd + '/random-1000-KBP.txt', 'r').read().splitlines()
allfiles = file(wd + '/all-sgm-files.txt', 'r').read().splitlines()

outfile = file('non-1000-files.txt', 'w')

set1000 = set(r1000)

for f in allfiles:
	if f not in set1000:
		outfile.write(f + '\n')

outfile.close()
