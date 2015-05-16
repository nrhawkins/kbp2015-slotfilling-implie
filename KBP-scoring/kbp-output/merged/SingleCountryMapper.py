
def construct_map(mapfile, lowercase=False):
  lines = file(mapfile, 'r').read().splitlines()
  if lowercase:
    lines = [l.lower() for l in lines]

  tokenlines = [l.split("\t") for l in lines]
  pairs = [(toks[0], toks[1].split(",")) for toks in tokenlines if len(toks) > 1]
  singles = [toks[0] for toks in tokenlines if len(toks) == 1]
  
  m = {}
  for name in singles:
    m[name] = name
  
  for name, alternates in pairs:
    newkeys = sorted(list(set(alternates + [name])))
    for newkey in newkeys:
      m[newkey] = name

  return m
  

class SingleCountryMapper:

  def __init__(self, mapfile, lowercase=False):
    self.mapper = construct_map(mapfile, lowercase)

  def getCountryName(self, name):
    return self.mapper.get(name, name)
