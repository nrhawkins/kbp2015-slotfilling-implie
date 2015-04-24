from unidecode import unidecode
import re

lines = file('subdivision_codes.csv', 'r').read().splitlines()

def include(line):
  tokens = line.split(',')
  if len(tokens) != 4:
    return False
  if line[0] != '\"' or line[len(line) - 1] != '\"':
    return False
  if 'city' in line.lower() or 'town' in line.lower() or 'saint' in line.lower():
    return False
  return True

names = [line.split(',')[2].replace('\"', '') for line in lines if include(line)]

names_no_brac = [re.sub("\[.*\]", "", name) for name in names]
#names_in_brac = [name[name.find("[") + 1:name.find("]")] for name in names if "[" in name and "]" in name]
#names = names_no_brac + names_in_brac
names = names_no_brac

names_no_paren = [re.sub("\(.*\)", "", name) for name in names]
#names_in_paren = [name[name.find("(") + 1:name.find(")")] for name in names if "(" in name and ")" in name]
#names_in_paren = [name for name in names_in_paren if len(name) > 2]
#names = names_no_paren + names_in_paren
names = names_no_paren

temp = names
names = []
for name in temp:
  names.extend(name.split("/"))

temp = names
names = []
for name in temp:
  names.extend(name.split(" - "))

decoded = []
for name in names:
  try:
    decoded.append(unidecode(unicode(name, 'utf-8')))
  except:
    try:
      decoded.append(unidecode(unicode(name, 'iso-8859-1')))
    except:
      print "Could not decode {}".format(name)


decoded = [name for name in decoded if '?' not in name]
decoded = [name.replace("*", "") for name in decoded]
decoded = [name.strip() for name in decoded]

# Remove entries that are first or last names in US census data.
firstnames = set(file('first_names.txt', 'r').read().splitlines())
lastnames = set(file('last_names.txt', 'r').read().splitlines())

decoded = [name for name in decoded if (name.lower() not in firstnames and name.lower() not in lastnames)]

# Remove entries that are normal english words.
words = set(file('enable1.txt', 'r').read().splitlines())
decoded = [name for name in decoded if name.lower() not in words]

out = file('country_subdivision_ascii', 'w')
out.write('\n'.join(decoded))
out.close()
