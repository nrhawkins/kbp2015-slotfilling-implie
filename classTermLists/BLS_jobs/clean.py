import re

def reverseComma(s):
  # If there is a single comma in the term, it is likely using the second
  # term as a specifier.  So remove comma and put specifier in the front.
  # Multiple commas indicate some sort of list, in which case this operation
  # is not appropriate.
  if s.count(',') != 1 or (s.count(',') == 1 and 'and' in s.split()):
    return s
  else:
    tokens = s.split(',')
    return tokens[1].strip() + ' ' + tokens[0].strip()

def removePlural(s):
  # REALLY basic... remove s at the end, if there is one.
  if s[len(s) - 3:] == 'ies':
    return s[:len(s) - 3] + 'y'
  elif s[len(s) - 1] == 's':
    return s[:len(s) - 1]
  else:
    return s

lines = file('jobsHTML.htm', 'r').read().splitlines()

lines = [line.strip() for line in lines if len(line.strip()) > 0]

lines = [re.sub(r'<h[^<>]*>[^<>]*<[^<>]*h[^<>]*>', r'', line) for line in lines]
lines = [re.sub(r'<[^<>]*>', r'', line) for line in lines]
lines = [re.sub(r', see.*', r'', line) for line in lines]

lines = [reverseComma(line) for line in lines]
lines = [line.strip() for line in lines if len(line.strip()) > 0]
lines = [removePlural(line) for line in lines]

lines = [line.lower() for line in lines]

lines = sorted(list(set(lines)))

out = file('az_jobs', 'w')
out.write('\n'.join(lines))
out.close()
