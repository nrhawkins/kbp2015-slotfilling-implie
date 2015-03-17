#!/usr/bin/python

import random

all_sents = 'all-sentence-file'
benchmark_sents = 'benchmark-subset-sentence-file'

elsefile = 'non-benchmark-sentence-file'
testfile = 'test-sentence-file'
test_size = 8500

allsenfile = file(all_sents, 'r')
bensenfile = file(benchmark_sents, 'r')
allsen = allsenfile.read().splitlines()
bensen = bensenfile.read().splitlines()

benset = set([line.split('\t')[0] for line in bensen])

# Write to file with all sentences except benchmark sentences.
elselst = [line for line in allsen if line.split('\t')[0] not in benset]
elseout = file(elsefile, 'w')
elseout.write('\n'.join(elselst))

# Write to file with test sentences.
testlst = [elselst[i] for i in sorted(random.sample(xrange(len(elselst)), test_size))]
testout = file(testfile, 'w')
testout.write('\n'.join(testlst))

allsenfile.close()
bensenfile.close()
elseout.close()
testout.close()
