#!/usr/bin/python

inf = file('3-answer-key.txt', 'r')
outf = file('3-slotfill-ignored-answer-key.txt', 'w')

lines = inf.read().splitlines()

tokenized = [line.split('\t') for line in lines]
for tokens in tokenized:
	tokens[4] = tokens[4].lower()

lined = ['\t'.join(tokens) for tokens in deduped]
deduped = set(lined)

outf.write('\n'.join(deduped))
outf.close()
inf.close()
