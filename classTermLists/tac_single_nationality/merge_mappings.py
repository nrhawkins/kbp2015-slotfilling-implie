
import sys

if len(sys.argv) < 4:
  sys.exit("Usage: python merge_mappings.py [map file 1] [map file 2] [output file]")

f1 = file(sys.argv[1], 'r').read().splitlines()
f2 = file(sys.argv[2], 'r').read().splitlines()

def make_map(lines):
  lines = [l for l in lines if l.strip() != ""]
  m = {}
  for line in lines:
    tokens = line.split("\t")
    name = tokens[0]
    if len(tokens) == 1:
      aliases = []
    else:
      aliases = tokens[1].split(",")
    try:
      m[name] = m.get(name, []) + aliases
    except:
      print m.get(name, [])
  return m

m1 = make_map(f1)
m2 = make_map(f2)

for k, v in m2.iteritems():
  m1[k] = m1.get(k, []) + v

out = file(sys.argv[3], 'w')
out.write("\n".join([name + "\t" + ",".join(aliases) for name, aliases in sorted(m1.iteritems())]))
out.close()


