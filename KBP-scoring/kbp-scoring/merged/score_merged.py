"""
Score merged output based on the openie and implie outputs alone.
Since the merged is a subset of the union of the two, there shouldn't be any missing.
"""

import sys
import re

if len(sys.argv) < 5:
  sys.exit("Usage: python score_merged.py [mergeid results] [openie scored] [implie scored] [merged scored output]")

valid_query_id = re.compile(r"SF[0-9][0-9]_ENG_[0-9][0-9][0-9]") 
query_id_length = 12

def load_scored(filename):
  lines = file(filename, 'r').read().splitlines()
  tokenlines = [(l.split("\t"), l) for l in lines]

  # Ignore if the second token is not a query id.
  tokenlines = [(toks, l) for toks, l in tokenlines if len(toks) > 2 and \
      re.findall(valid_query_id, toks[1]) and len(toks[1]) == query_id_length]

  toline_map = {}
  for tokens, line in tokenlines:
    queryid = tokens[1]
    slotname = tokens[3]
    slotfill = tokens[4].lower()
    # Turn to answer key tuple
    tup = (queryid, slotname, slotfill)
    toline_map[tup] = line

  return toline_map


results = file(sys.argv[1], 'r').read().splitlines()
o_toline_map = load_scored(sys.argv[2])
i_toline_map = load_scored(sys.argv[3])
out = file(sys.argv[4], 'w')

results = [r.split("\t") for r in results if r.split("\t")[3].strip() != "NIL"]
result_tuples = [(tup[0], tup[1], tup[4].lower()) for tup in results]

lines = []
notfound = []
for tup in result_tuples:
  line = i_toline_map.get(tup, o_toline_map.get(tup))
  if line is None:
    notfound.append(tup)

  lines.append(line)


out.write("\t\t\t\t\tofficial scoring\t\t\tour scoring\n")
headers = ["file id", "query id", "entity", "slotname", "slotfill", "correct", \
    "inexact", "incorrect", "correct", "inexact", "incorrect", "offset1", "offset2", "context"]
out.write("\t".join(headers) + "\n")

out.write("\n".join(lines))

out.write("\n\nNot Found (Probably duplicates)\n")
out.write("\n".join(notfound))

out.close()

