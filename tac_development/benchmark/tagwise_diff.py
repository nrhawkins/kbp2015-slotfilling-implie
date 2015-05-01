#!/usr/bin/python

import sys
import string

def makeMap(lines):
    mp = {}
    for l in lines:
        tokens = l.split('\t')
        # Match on index, relation, tag and correctness.
        if tokens[0].isdigit():
            mp[(tokens[0],tokens[3],tokens[4],tokens[5],tokens[6])] = l
    return mp

indir = 'scoring'
outdir = 'diff-scoring'
intemplate = '{}-extraction-scoring'
outtemplate = '{}-{}-tagwise-diff.txt'

file1 = file(indir + '/' + intemplate.format(sys.argv[1]), 'r')
file2 = file(indir + '/' + intemplate.format(sys.argv[2]), 'r')

mp1 = makeMap(file1.read().splitlines())
mp2 = makeMap(file2.read().splitlines())

outfile = file(outdir + '/' + outtemplate.format(sys.argv[1], sys.argv[2]), 'w')

# 	          0		        1	  2		  3			4	  5		  6				7
# format: Sentence Index, docID, entity, relation, tag,	correct, incorrect,	sentence

# Different Score
outfile.write('Extractions with the same relation but different scores.\n')
m1keys = []
m2keys = []
for k, v in mp1.iteritems():
    (si, rel, tag, cor, incor) = k
    # flip the correct and incorrect for the different score.
    if (si, rel, tag, incor, cor) in mp2:
        outfile.write(v + '\n')
        outfile.write(mp2[(si, rel, tag, incor, cor)] + '\n')
        outfile.write('\n')
        m1keys.append(k)
        m2keys.append((si, rel, tag, incor, cor))

for k in m1keys:
    del mp1[k]
for k in m2keys:
    del mp2[k]

outfile.write('\n\n\n')

# Same Score
outfile.write('Extractions with the same relation and same scores.\n')
m1keys = []
m2keys = []
for k, v in mp1.iteritems():
    # flip the correct and incorrect for the different score.
    if k in mp2:
        outfile.write(v + '\n')
        outfile.write(mp2[k] + '\n')
        outfile.write('\n')
        m1keys.append(k)
        m2keys.append(k)

for k in m1keys:
    del mp1[k]
for k in m2keys:
    del mp2[k]

outfile.write('\n\n\n')

# Only found in one.
outfile.write('Extractions with relations only found in one of the results.\n')
outfile.write('file 1\n')
for k, v in mp1.iteritems():
    outfile.write(v + '\n')
outfile.write('\n')
outfile.write('file 2\n')
for k, v in mp2.iteritems():
    outfile.write(v + '\n')


file1.close()
file2.close()
outfile.close()
