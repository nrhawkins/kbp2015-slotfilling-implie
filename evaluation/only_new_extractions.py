import sys
from collections import namedtuple

"""
Lists only the extractions that did not already exist in the old extractions 
file based on the {sentence index, document id, entity, relation, slotfill, 
sentence}.

All files are tab delimited files.
"""

Extraction = namedtuple('extraction', ['sentence_index', 'doc_id', 'entity', \
    'relation', 'slotfill', 'correct', 'incorrect', 'sentence'])
ComparisonExtraction = namedtuple('comparison_extraction', ['sentence_index', \
    'doc_id', 'entity', 'relation', 'slotfill', 'sentence'])

if len(sys.argv) < 5:
  sys.exit("Usage: python only_new_extractions.py [old extractions file] [new extractions file] [only new output file] [only old output file]")

def string_is_int(string):
  try:
    int(string)
    return True
  except:
    return False

"""
A line counts as an extraction if there are 8 tab-delimited tokens and the
first token is an int.
"""
def is_extraction(line):
  tokens = line.split('\t')
  return len(tokens) == 8 and string_is_int(tokens[0])

def construct_extrset(lines):
  extrlines = [line for line in lines if is_extraction(line)]
  complist = []
  for line in extrlines:
    ts = line.split('\t')
    sentence = ts[7]
    # Remove quotes from sentence if it's on both sides.
    #if sentence[0] == "\"" and sentence[len(sentence) - 1] == "\"":
    #  sentence = str(sentence[1:len(sentence) - 1])
    sentence = sentence.replace("\"", "")
    if sentence[len(sentence) - 1] == ".":
      sentence = str(sentence[:len(sentence) - 1])


    slotfill = ts[4].lower() # The new ones are all lowercase, so make them match.

    # Skip correct and incorrect.
    complist.append(ComparisonExtraction(ts[0], ts[1], ts[2], ts[3], slotfill, sentence))
  return frozenset(complist)

def construct_newlist_newmap(lines):
  extrlines = [line for line in lines if is_extraction(line)]

  newmap = {}
  newlist = []
  for line in extrlines:
    ts = line.split('\t')
    extraction = Extraction(ts[0], ts[1], ts[2], ts[3], ts[4], ts[5], ts[6], ts[7])
    comparison_extraction = ComparisonExtraction(ts[0], ts[1], ts[2], ts[3], ts[4], ts[7])

    newmap[comparison_extraction] = extraction
    newlist.append(comparison_extraction)
  return newlist, newmap

print 'old file {}'.format(sys.argv[1])
print 'new file {}'.format(sys.argv[2])
print 'output file {}'.format(sys.argv[3])

old = file(sys.argv[1], 'r').read().splitlines()
new = file(sys.argv[2], 'r').read().splitlines()

oldset = construct_extrset(old)
newset = construct_extrset(new)
newlist, newmap = construct_newlist_newmap(new)

"""
onlynew = []
for newextraction in newlist:
  if newextraction not in oldset:
    onlynew.append(newmap[newextraction])

headers = ["SentenceIndex", "DocumentId", "Entity", "Relation",\
    "Slotfill(tag)", "Correct", "Incorrect", "Sentence"]
"""

inter = oldset & newset
onlynew = sorted(list(newset - inter))
onlyold = sorted(list(oldset - inter))

delim = "\t"
out = file(sys.argv[3], 'w')
#out.write(delim.join(headers) + "\n")
newlines = [delim.join(list(extraction)) for extraction in onlynew]
out.write("\n".join(newlines))
out.close()

outold = file(sys.argv[4], 'w')
oldlines = [delim.join(list(extraction)) for extraction in onlyold]
outold.write("\n".join(oldlines))
outold.close()

