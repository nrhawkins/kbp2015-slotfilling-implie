
import sys


f = file(sys.argv[1], 'r').read().splitlines()

lines = [l for l in f if "NIL" not in l]
lines = [l for l in lines if len(l.split()) > 0 and len(l.split()[0].strip()) == 1]

out = file(sys.argv[2], 'w')
out.write("\n".join(lines))
out.close()
