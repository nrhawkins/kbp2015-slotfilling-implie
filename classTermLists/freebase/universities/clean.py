
lines = file("freebase_universities", "r").read().splitlines()

lines = [line.split(",")[0] for line in lines if line != ""]

lines = [line for line in lines if "football" not in line.lower()]

# remove entries that are first or last names in us census data.
firstnames = set(file('../city/first_names.txt', 'r').read().splitlines())
lastnames = set(file('../city/last_names.txt', 'r').read().splitlines())

lines = [line for line in lines if (line.lower() not in firstnames and line.lower() not in lastnames)]

# remove entries that are normal english words.
words = set(file('../city/enable1.txt', 'r').read().splitlines())
lines = [line for line in lines if line.lower() not in words]

lines = [line.strip() for line in lines]

lines = [line for line in lines if len(line) > 2]

lines = list(set(lines)) # uniquify.

out = file("cleaned_universities", "w")
out.write("\n".join(lines))
out.close()
