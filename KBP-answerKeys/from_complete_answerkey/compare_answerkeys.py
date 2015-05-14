
import sys

if len(sys.argv) < 4:
    sys.exit("Requires at least 3 arguments.")

file1 = file(sys.argv[1], 'r').read().splitlines()[1:] # 1st file has header.
file2 = file(sys.argv[2], 'r').read().splitlines()

tuples1 = set([tuple(line.split("\t")) for line in file1])
tuples2 = set([(tup[0], tup[2], tup[3]) for tup in [tuple(line.split("\t")) for line in file2]])

only1 = sorted(tuples1 - tuples2)
only2 = sorted(tuples2 - tuples1)
both = sorted(tuples1 & tuples2)

out = file(sys.argv[3], 'w')
out.write("file1 {}, file2 {}, both {}\n".format(len(only1), len(only2), len(both)))
out.write("Only file1\n")
out.write("\n".join([str(s) for s in only1]))
out.write("\n\n\n")
out.write("Only file2\n")
out.write("\n".join([str(s) for s in only2]))
out.write("\n\n\n")
out.write("Both\n")
out.write("\n".join([str(s) for s in both]))
out.write("\n\n\n")
out.close()
