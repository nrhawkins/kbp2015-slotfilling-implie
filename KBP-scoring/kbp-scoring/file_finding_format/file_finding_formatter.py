"""
Reformats a KBP submission file to a format that can be used
for one of Stephen's scripts to find the context sentence.

The file it formats for is /projects/WebWare6/DEFT_corpora/TAC_2013/LDC2014E45_TAC_2013_KBP_Source_Corpus_disc_2/data/English/findSenteceText2014.pl
"""

import sys
import re

if len(sys.argv) < 3:
  sys.exit("Usage: python file_finding_formatter.py [original results file] [query to entity file] [output file]")

def query_to_entity_map(filename):
  lines = file(filename, 'r').read().splitlines()
  m = {}
  for l in lines:
    toks = l.split("\t")
    if len(toks) < 2:
      continue
    m[toks[0]] = toks[1]
  return m

lines = file(sys.argv[1], 'r').read().splitlines()
qtoe_map = query_to_entity_map(sys.argv[2])
out = file(sys.argv[3], 'w')

valid_query_id = re.compile(r"SF[0-9][0-9]_ENG_[0-9][0-9][0-9]") 
query_id_length = 12

state_change_map = {"Ignore":("Correct results", "C"),"Correct results":("Inexact results","X"),"Inexact results":("Incorrect results", "I"),"Incorrect results":("Summary","S")}
state = "Ignore"
state_char = "Ig"
outstrs = []
for i in range(len(lines)):
  line = lines[i]

  if line.strip() == "":
    continue

  # Change state.
  if line.strip() == state_change_map.get(state, "NOT any LinE")[0]:
    state, state_char = state_change_map[state]

  tokens = line.split("\t")

  # Ignore if the first token isn't a query id.
  if not (re.findall(valid_query_id, tokens[0])\
      and len(tokens[0]) == query_id_length):
    continue

  # Continue if it's in the ignore state.
  if state == "Ignore":
    continue

  query = tokens[0]
  slotname = tokens[1]
  filename = tokens[3]
  slotfill = tokens[4]
  offsets = tokens[5].split("-")
  confidence = tokens[len(tokens) - 1]

  # Output string.
  outtokens = [filename[:14], filename, query, qtoe_map[query], slotname, slotfill,\
      state_char, offsets[0], offsets[1], "", "", "", "", "", "", "EOL"]
  outstring = "\t".join(outtokens)
  outstrs.append(outstring)

out.write("\n".join(outstrs))
out.close()

