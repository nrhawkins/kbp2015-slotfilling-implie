import sys

lines = file(sys.argv[1], 'r').read().splitlines()

lines = [line for line in lines if len(line.strip()) < 4]

out = file(sys.argv[2], 'w')
out.write('\n'.join(lines))
out.close()
