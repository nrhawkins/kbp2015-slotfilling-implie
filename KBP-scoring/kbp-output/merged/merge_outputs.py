
import sys
from collections import defaultdict
from SingleCountryMapper import SingleCountryMapper

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
country_slots = ["per:origin", "per:countries_of_residence", "org:country_of_headquarters"]

countrymapper = SingleCountryMapper("extended_country_mapping", lowercase=True)

# Extractions first identified by the queryid, then the slotfill
# Output files are organized such that
# 0:queryid, 1:slotfill, 2:UWashington2, 3:Nil or starting bound, 4:extraction, 5:end bound, 6:confidence

def tokenline(lst):
  return [(line.split("\t"), line) for line in lst]
otokenlines = tokenline(ofile)
itokenlines = tokenline(ifile)

extractions = defaultdict(dict)

# Add openie results.
for tokens, line in otokenlines:
  queryid = tokens[0]
  slotfill = tokens[1]
  nilentry = tokens[3]
  
  querydict = extractions[queryid]
  entries = querydict.get(slotfill, [])
  
  if nilentry == "NIL":
    if len(entries) == 0:
      querydict[slotfill] = [(tokens, line)]
    # Otherwise ignore it.
  else:
    if len(entries) == 0:
      querydict[slotfill] = [(tokens, line)]
    elif len(entries) == 1 and entries[0][0][3] == "NIL":
      querydict[slotfill] = [(tokens, line)]
    else:
      if slotfill in single_slots:
        assert(len(entries) == 1)
        confidence_old = float(entries[0][0][6])
        confidence_new = float(tokens[6])
        if confidence_new > confidence_old:
          querydict[slotfill] = [(tokens, line)]
      else:
        entries.append((tokens, line))

# Add implie results.
for tokens, line in itokenlines:
  queryid = tokens[0]
  slotfill = tokens[1]
  nilentry = tokens[3]
  
  querydict = extractions[queryid]
  entries = querydict.get(slotfill, [])
  
  if nilentry == "NIL":
    if len(entries) == 0:
      querydict[slotfill] = [(tokens, line)]
    # Otherwise ignore it.
  else:
    if len(entries) == 0:
      querydict[slotfill] = [(tokens, line)]
    elif len(entries) == 1 and entries[0][0][3] == "NIL":
      querydict[slotfill] = [(tokens, line)]
    else:
      if slotfill in single_slots:
        assert(len(entries) == 1)
        # This is a bit of a hack.
        # Since the openie confidences are [0,1], and implie confidences  
        # are >= 0.8, this will always result in an implie result with a highest
        # count given that there is an implie result.
        confidence_old = round(float(entries[0][0][6]))
        confidence_new = round(float(tokens[6]))
        if confidence_new >= confidence_old:
          querydict[slotfill] = [(tokens, line)]
      else:
        entries.append((tokens, line))


# Remove duplicates, use the single country mapper for country relations.
deduped_extractions = {}
for query, slotfillmap in extractions.iteritems():
  deduped_extractions[query] = {}
  for slotfill, extractionlist in slotfillmap.iteritems():
    # No duplicates to eliminate.
    if len(extractionlist) <= 1:
      deduped_extractions[query][slotfill] = extractionlist
      continue

    # Create a new map where you key the token/line by the entity.
    # For country slots use the single country mapping.
    # This will eliminate any duplicates, and pull out the
    # remaining as the extraction list.
    entitymap = {}
    for tokens, line in extractionlist:
      entity = tokens[5].lower()
      if slotfill in country_slots:
        entity = countrymapper.getCountryName(entity)
      entitymap[entity] = (tokens, line)

    deduped_list = [v for k, v in entitymap.iteritems()]
    deduped_extractions[query][slotfill] = deduped_list


lines = [[[line for tokens, line in extractionlist] for slotfill, extractionlist in slotfillmap.iteritems()] for query, slotfillmap in deduped_extractions.iteritems()]
lines = [l for lll in lines for ll in lll for l in ll]
lines = sorted(lines)

print lines[:5]

out = file(sys.argv[3], 'w')
out.write("\n".join(lines))
out.close()

