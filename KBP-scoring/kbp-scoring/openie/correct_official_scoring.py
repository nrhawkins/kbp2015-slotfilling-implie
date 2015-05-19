
import sys

current = file(sys.argv[1], 'r').read().splitlines()
official = file(sys.argv[2], 'r').read().splitlines()
out = file(sys.argv[3], 'w')


# Make correct map.

# For 2013
m = {}
for l in official:
  if len(l.split()) == 0 or len(l.split()[0]) != 1:
    continue
  space_toks = l.split()
  result = space_toks[0].upper()
  colsplit = space_toks[1].split(":")
  query = colsplit[0]
  slotname = ":".join(colsplit[1:])
  lcolsplit = l.split(":")
  slotfill = lcolsplit[len(lcolsplit) - 1]

  m[(query, slotname, slotfill.lower().strip())] = result

newlines = []
for l in current:
  tokens = l.split("\t")
  if len(tokens[1]) != 12 or len(tokens) != 14:
    newlines.append(l)
    continue

  for i in range(5, 8):
    tokens[i] = ""

  result = m.get((tokens[1], tokens[3], tokens[4].lower()))
  if result is None:
    print m
    sys.exit("NONE... {}".format((tokens[1], tokens[3], tokens[4].lower())))
  if result == "C":
    tokens[5] = "1"
  elif result == "X":
    tokens[6] = "1"
  elif result == "W":
    tokens[7] = "1"
  elif result == "R":
    tokens[5] = "1"
  else:
    sys.exit("WTF... {}".format(result))

  newlines.append("\t".join(tokens))

out.write("\n".join(newlines))

out.close()
