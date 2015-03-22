#!/usr/bin/python

import random

all_sents = 'new-1000-file-sentences'
testfile = 'test-sentence-subset'
test_size = 9000

allsenfile = file(all_sents, 'r')
allsen = allsenfile.read().splitlines()

# Write to file with test sentences.
testlst = [allsen[i] for i in sorted(random.sample(xrange(len(allsen)), test_size))]
testout = file(testfile, 'w')
testout.write('\n'.join(testlst))

allsenfile.close()
testout.close()
