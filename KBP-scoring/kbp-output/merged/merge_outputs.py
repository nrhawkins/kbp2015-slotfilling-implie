
import sys
from collections import defaultdict

if len(sys.argv) < 4:
  sys.exit("Usage: python merge_outputs.py [openie output] [implie output] [merged file]")

ofile = file(sys.argv[1], 'r').read().splitlines()
ifile = file(sys.argv[2], 'r').read().splitlines()

"""
For people extractions:
  origin, countries_of_residence, cities_of_residence, statesorprovinces_of_residence, title, religion
  All can be lists so merge

For organization extractions:
  country_of_headquarters, province_of_headquarters, city_of_headquarters [can only have 1, so take ImplIE's if there's a conflict]
  top_members_employees, political_religion_affiliation (just merge)

"""

merging_slots = ["per:origin", "per:countries_of_residence", "per:cities_of_residence", \
    "per:statesorprovinces_of_residence", "per:title", "per:religion", "org:top_members_employees", \
    "org:political_religion_affiliation"]
single_slots = ["org:country_of_headquarters", "org:province_of_headquarters", "org:city_of_headquarters"]

# Extractions first identified by the queryid, then the slotfill
# Output files are organized such that
# 0:queryid, 1:slotfill, 2:UWashington2, 3:Nil or starting bound, 4:extraction, 5:end bound, 6:confidence

def tokenline(lst):
  return [(line.split("\t"), line) for line in lst]
otokenlines = tokenline(ofile)
itokenlines = tokenline(ifile)

extractions = defaultdict(dict)

for tokens, line in otokenlines:
  queryid = tokens[0]
  slotfill = tokens[1]
  nilentry = tokens[3]
  
  querydict = extractions[queryid]
  entries = querydict.get(slotfill, [])
  
  if nilentry == "NIL":
    if len(entries) == 0:
      querydict[slotfill] = [(tokens, line)]
  else:
    # Append to the querydict





##
## REMEMBER, to use a single country mapper on openie to avoid duplicates.
##





