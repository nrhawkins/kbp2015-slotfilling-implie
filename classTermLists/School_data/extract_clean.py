
lines = file('school_data.csv','r').read().splitlines()[1:] # first line is the header.

lines = [line.replace('\"', '') for line in lines]

temp = lines
lines = []
for line in temp:
  tokens = line.split(',')
  lines.append(tokens[1])
  lines.append(tokens[4])

temp = lines
lines = []
for line in temp:
  tokens = line.split('|')
  lines.extend(tokens)

temp = lines
lines = []
for line in temp:
  tokens = line.split(';')
  lines.extend(tokens)

temp = lines
lines = []
for line in temp:
  tokens = line.split('/')
  lines.extend(tokens)

lines = [line.strip() for line in lines if len(line.strip()) > 2]

# remove entries that are first or last names in us census data.
firstnames = set(file('../freebase/city/first_names.txt', 'r').read().splitlines())
lastnames = set(file('../freebase/city/last_names.txt', 'r').read().splitlines())

lines = [line for line in lines if (line.lower() not in firstnames and line.lower() not in lastnames)]

# remove entries that are normal english words.
words = set(file('../freebase/city/enable1.txt', 'r').read().splitlines())
lines = [line for line in lines if line.replace("the","").replace("The","").strip().lower() not in words]

# remove if completely lowercase.
lines = [line for line in lines if line.lower() != line]

lines = sorted(list(set(lines))) # make unique and sort.

out = file('schools','w')
out.write('\n'.join(lines))
out.close()
