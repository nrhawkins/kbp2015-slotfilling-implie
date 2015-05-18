
import sys
import xml.etree.ElementTree as ET

if len(sys.argv) < 3:
  sys.exit("Usage: python query_to_entityname.py [input xml file] [output file]")

tree = ET.parse(sys.argv[1])
root = tree.getroot()

query_name_list = []
for child in root:
  assert(child.tag == "query")
  queryid = child.attrib["id"]
  
  assert(child[0].tag == "name")
  entity = child[0].text
  query_name_list.append((queryid, entity))

strtuples = [queryid + "\t" + entity for queryid, entity in query_name_list]

out = file(sys.argv[2], 'w')
out.write("\n".join(strtuples))
out.close()
