#!/usr/bin/python

import sys
import re

if len(sys.argv) < 3:
  sys.exit("Usage: python clean_cities.py [input file] [output file]") 

inputfile = sys.argv[1]
outputfile = sys.argv[2]

lines = file(inputfile, 'r').read().splitlines()
out = file(outputfile, 'w')

# Remove substrings in parentheses.
lines = [re.sub("\(.*\)", "", line) for line in lines]

# Split slash separated cities.
sep = []
for city in lines:
    sep.extend(city.split('/'))

# Remove ones that contain the word 'county'
lines = [term for term in sep if 'county' not in term.lower()]

# Keep only ones that are at least 2 characters.
lines = [city for city in lines if len(city) > 1]

# Removed entries taht contain a number.
# Helper function.
def no_nums(string):
  return not any(i.isdigit() for i in string)
lines = [term for term in lines if no_nums(term)]

print 'after no nums {}'.format(len(lines))

# Take only the first term for any that are comma separated.
lines = [tokens.split(",")[0] for tokens in lines]

# Fix capitalization if all caps.
capfixed = []
for lines in lines:
    tokens = lines.split()
    if ''.join(tokens).isupper():
        capfixed.append(['{}{}'.format(token[0].upper(), token[1:].lower()) for token in tokens])
    else:
        capfixed.append(tokens)

# Remove duplicates.
cityset = list(set([' '.join(tokens) for tokens in capfixed]))

print 'duplicates {}'.format(len(cityset))

# Remove surrounding quotes.
# Replace @ with a, capitalize if the first letter.
temp = cityset
cityset = []
for line in temp:
  l = line
  if line[0] == '\"' and line[len(line) - 1] == '\"':
    l = line[1:len(line) - 1]
  
  if l[0] == '@':
    l = 'A' + l[1:]
  l = l.replace('@', 'a')

  cityset.append(l)

print 'before vowels {}'.format(len(cityset))

# Remove if all lowercase.  (With freebase all lowercase generally means the
# entry was hastily of badly done).
cityset = [city for city in cityset if not city == city.lower()]

print 'after vowels {}'.format(len(cityset))

# Remove if it contains dirty strings.
dirty = ['www.', '.com', '.edu', '.org', '.gov']
# helper
def isdirty(string):
  for w in dirty:
    if w in string:
      return True
  return False

cityset = [city for city in cityset if not isdirty(city.lower())]

print 'after dirty {}'.format(len(cityset))

# remove entries that are first or last names in us census data.
firstnames = set(file('first_names.txt', 'r').read().splitlines())
lastnames = set(file('last_names.txt', 'r').read().splitlines())

cityset = [city for city in cityset if (city.lower() not in firstnames and city.lower() not in lastnames)]

# remove entries that are normal english words.
words = set(file('enable1.txt', 'r').read().splitlines())
cityset = [city for city in cityset if city.lower() not in words]

# Sort.
cityset = sorted(cityset)

# trim
cityset = [city.strip() for city in cityset if city != '']

# Keep only ones that are at least 2 characters.
cityset = [city for city in cityset if len(city) > 1]

print 'end {}'.format(len(cityset))

out.write('\n'.join(cityset))

out.close()
