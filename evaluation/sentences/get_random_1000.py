#!/usr/bin/python

import random

allfs = file('non-benchmark-files.txt', 'r').read().splitlines()
out = file('random-1000-testfiles.txt', 'w')

num = 1000

for i in range(0, num):
	index = random.randint(0, len(allfs))
	out.write(allfs[index] + '\n')
	del allfs[index]

out.close()
