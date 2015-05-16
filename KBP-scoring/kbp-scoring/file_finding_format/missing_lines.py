
import sys

if len(sys.argv) < 4:
  sys.exit("Usage: python ....py [before file finding] [after file finding] [output file]")

f1 = file(sys.argv[1], 'r').read().splitlines()
f2 = file(sys.argv[2], 'r').read().splitlines()
out = file(sys.argv[3], 'w')

i1 = 0
i2 = 0
for i in range(len(f1)):
  i1 = i

  toks1 = f1[i1].split("\t")
  toks2 = f2[i2].split("\t")

  if toks1[1] != toks2[0]:
    out.write(f1[i1] + "\n")
  else:
    i2 += 1

out.close()
