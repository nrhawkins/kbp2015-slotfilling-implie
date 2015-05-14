
import sys

if len(sys.argv) < 4:
    sys.exit("Requires at least 3 arguments.")

file1 = file(sys.argv[1], 'r').read().splitlines()[1:] # 1st file has header.
file2 = file(sys.argv[2], 'r').read().splitlines()

tuples1 = set([tuple(line.split("\t")) for line in file1])
tuples2 = set([(tup[0], tup[2], tup[3]) for tup in [tuple(line.split("\t")) for line in file2]])

union = sorted(tuples1 | tuples2)

out = file(sys.argv[3], 'w')

out.write("\n".join(["{}\t{}\t{}".format(t[0], t[1], t[2]) for t in union]))
out.close()
