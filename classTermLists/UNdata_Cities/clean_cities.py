#!/usr/bin/python

lines = file('UN_dirty_citylist.csv', 'r').read().splitlines()
out = file('UN_citylist.csv', 'w')

tokenlist = [line.split(' ') for line in lines]

# Remove tokens in parentheses.
tokenlist = [[token for token in tokens if ('(' not in token and ')' not in token)] for tokens in tokenlist]

# Fix capitalization if all caps.
capfixed = []
for tokens in tokenlist:
    if ''.join(tokens).isupper():
        capfixed.append(['{}{}'.format(token[0].upper(), token[1:].lower()) for token in tokens])
    else:
        capfixed.append(tokens)

# Remove duplicates.
cityset = list(set([' '.join(tokens) for tokens in capfixed]))

# Split slash separated cities.
sep = []
for city in cityset:
    sep.extend(city.split('/'))

# trim
cityset = [city.strip() for city in sep if city is not '']

out.write('\n'.join(cityset))

out.close()
