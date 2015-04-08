#!/usr/bin/python
from unidecode import unidecode

cities = file('UN_citylist.csv', 'r').read().splitlines()
out = file('UN_citylist_ascii.csv', 'w')

trans = [unidecode(unicode(city, 'utf-8')) for city in cities]

out.write('\n'.join(trans))
out.close() 
