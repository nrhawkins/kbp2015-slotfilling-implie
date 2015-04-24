lines = file('schools', 'r').read().splitlines()

lines = [line for line in lines if len(line.strip().split()) == 1]

lines = sorted(lines)

out = file('singlewords', 'w')
out.write('\n'.join(lines))
out.close()
