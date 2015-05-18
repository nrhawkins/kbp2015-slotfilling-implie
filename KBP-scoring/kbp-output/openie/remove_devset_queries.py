
import sys

if len(sys.argv) < 4:
  sys.exit("Usage: python remove_devset_queries.py [input file] [year] [out file]")


devsets = {2013:["004", "011", "027", "034", "050", "080", "083", "087", "091", "095"], \
    2014:["001", "012", "038", "039", "046", "068", "079", "082", "088", "092"]}
yearstrs = {2013:"13", 2014:"14"}

devset_nums = devsets.get(int(sys.argv[2]))
yearstr = yearstrs.get(int(sys.argv[2]))

if devset_nums is None:
  sys.exit("Invalid devset year: {}".format(sys.argv[2]))


devset = ["SF{}_ENG_{}".format(yearstr, num) for num in devset_nums]

lines = file(sys.argv[1], 'r').read().splitlines()
lines = [l for l in lines if l.split("\t")[0] not in devset]

out = file(sys.argv[3], 'w')
out.write("\n".join(lines))
out.close()

