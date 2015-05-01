import string

class SenKey:
	def __init__(self, index, docid):
		self.index = index
		self.docid = docid

def makeMap(sentences):
	mp = {}
	for s in sentences:
		tokens = s.split('\t')
		mp[tokens[7]] = SenKey(tokens[0], tokens[1])
	return mp

# For each line in the first file, replace the first two tokens with 
# the first two tokens in the second file where the last token matches.
#					0								1			2					3				4			5				6						7
# format: Sentence Index, docID, entity, relation, tag,	correct, incorrect,	sentence
def main():
	broken = open('scoring/initial-answer-key.txt', 'r')
	old = open('/homes/gws/genelkim/5-answer-key.txt', 'r')
	out = open('scoring/correct-answer-key.txt', 'w')
	bl = broken.read().splitlines()
	ol = old.read().splitlines()

	out.write(bl[0] + "\n")
	bl = bl[1:]

	mp = makeMap(ol)

	for s in bl:
		tokens = s.split('\t')
		senkey = mp[tokens[7]]
		out.write("{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n".format(\
			senkey.index, senkey.docid, tokens[2], tokens[3], tokens[4],\
			tokens[5], tokens[6], tokens[7]))
		
	out.close()


if __name__ == "__main__":
  main()
