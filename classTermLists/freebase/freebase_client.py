#!/usr/bin/python

import json
import urllib
from unidecode import unidecode

import sys

if len(sys.argv) == 1:
  sys.exit("Usage: python freebase_client.py [output file name]")


outfile_name = sys.argv[1]

#api_key = "AIzaSyAA3JMW5g32RfdtwroF9VEf_GcBBDEVspA"
api_key = "AIzaSyBfBytAi-_87cRCa3erhp-R4Uq6KwLQ2hk"
service_url = 'https://www.googleapis.com/freebase/v1/mqlread'
#query = [{'id': None, 'name': None, 'type': '/people/profession'}]
#query = [{'id': None, 'name': None, 'collective_term_for_adherents': None, 'type': '/religion/religion'}]

# Cities.
#query = [{'id': None, 'name': None, 'type': '/location/citytown', \
#  '/location/location/adjectival_form': [{'value': None, 'lang': '/lang/en', 'optional': True}],\
#  '/common/topic/alias': [{'value': None, 'lang': '/lang/en', 'optional': True}]}]

# Countries.
query = [{'id': None, 'name': None, 'type': '/location/country', \
  '/location/location/adjectival_form': [{'value': None, 'lang': '/lang/en', 'optional': True}],\
  '/common/topic/alias': [{'value': None, 'lang': '/lang/en', 'optional': True}]}]

params = {
#  'query': json.dumps(query)
  'query': json.dumps(query),
  'key': api_key
}
cursor = ""

lst = []
for i in range(0, 50000):
  print cursor
  url = service_url + '?' + urllib.urlencode(params) + "&cursor" + cursor
  response = json.loads(urllib.urlopen(url).read())

  # All queries.
  lst.extend([item['name'] for item in response['result'] if item['name'] is not None])


  # City/Country query.
  #lst.extend([adj['value'] for item in response['result'] for adj in item['/location/location/adjectival_form'] if adj['value'] is not None])
  #lst.extend([alias['value'] for item in response['result'] for alias in item['/common/topic/alias'] if alias['value'] is not None])

  # Religion
  #lst.extend([profession['collective_term_for_adherents'] for profession in response['result'] if profession['collective_term_for_adherents'] is not None])

  cursor = response['cursor']
  if cursor is False:
    break
  cursor = "=" + response['cursor']

out = file(outfile_name, 'w')
out.write(unidecode('\n'.join(frozenset(lst))))

print len(lst)

out.close()
