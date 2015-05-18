
import sys
from collections import defaultdict
import copy

if len(sys.argv) < 3:
  sys.exit("Usage: python ...py [input results file] [output results file]")

residence_key = "per:countries_of_residence"
origin_key = "per:origin"

lines = file(sys.argv[1], 'r').read().splitlines()

query_results = defaultdict(list)
for line in lines:
  queryid = line.split("\t")[0]
  query_results[queryid].append(line)

newlines = []
for k, v in query_results.iteritems():
  residences = []
  origins = []
  others = []
  tokenlines = [line.split("\t") for line in v]
  for tokens in tokenlines:
    if tokens[1] == residence_key:
      residences.append(tokens)
    elif tokens[1] == origin_key:
      origins.append(tokens)
    else:
      others.append(tokens)

  no_origins = True
  for origin in origins:
    if origin[3] != "NIL":
      no_origins = False

  if no_origins:
    origins = copy.deepcopy(residences)
    for origin in origins:
      origin[1] = origin_key

  newlines.extend(["\t".join(tokens) for tokens in others])
  newlines.extend(["\t".join(tokens) for tokens in residences])
  newlines.extend(["\t".join(tokens) for tokens in origins])

newlines = sorted(newlines)

out = file(sys.argv[2], 'w')
out.write("\n".join(newlines))
out.close()
