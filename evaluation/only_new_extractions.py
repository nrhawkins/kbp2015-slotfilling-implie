import sys
import collections.namedtuple

"""
Lists only the extractions that did not already exist in the old extractions 
file based on the {sentence index, document id, entity, relation, slotfill, 
sentence}.

All files are tab delimited files.
"""

Extraction = namedtuple('extraction', ['sentence_index', 'doc_id', 'entity', \
    'relation', 'slotfill', 'correct', 'incorrect', 'sentence'])
ComparisonExtraction = namedtuple('comparison extraction', ['sentence_index', \
    'doc_id', 'entity', 'relation', 'slotfill', 'sentence'])

if len(sys.argv) < 4:
  sys.exit("Usage: python only_new_extractions.py [old extractions file] [new extractions file] [output file]")

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

def construct_oldset(lines):
  extrlines = [line for line in lines if is_extraction(line)]
  complist = []
  for line in extrlines:
    ts = lines.split('\t')
    # Skip correct and incorrect.
    complist.append(ComparisonExtraction(ts[0], ts[1], ts[2], ts[3], ts[4], ts[7]))
  return frozenset(complist)

def construct_newlist_newmap(lines):
  extrlines = [line for line in lines if is_extraction(line)]

  newmap = {}
  newlist = []
  for line in extrlines:
    extraction = Extraction(ts[0], ts[1], ts[2], ts[3], ts[4], ts[5], ts[6], ts[7]))
    comparison_extraction = ComparisonExtraction(ts[0], ts[1], ts[2], ts[3], ts[4], ts[7]))

    newmap[comparison_extraction] = extraction
    newlist.append(comparison_extraction)
  return newlist, newmap

print 'old file {}'.format(sys.argv[1])
print 'new file {}'.format(sys.argv[2])
print 'output file {}'.format(sys.argv[3])

old = file(sys.argv[1], 'r').read().splitlines()
new = file(sys.argv[2], 'r').read().splitlines()

oldset = construct_oldset(old)
newlist, newmap = construct_newlist_newmap(new)

onlynew = []
for newextraction in newlist:
  if newextraction not in oldset:
    onlynew.append(newmap[newextraction])

headers = ["SentenceIndex", "DocumentId", "Entity", "Relation",\
    "Slotfill(tag)", "Correct", "Incorrect", "Sentence"]

delim = "\t"
out = file(sys.argv[3], 'w')
out.write(delim.join(headers) + "\n")
newlines = [delim.join(list(extraction)) for extraction in onlynew]
out.write("\n".join(newlines))
out.close()

