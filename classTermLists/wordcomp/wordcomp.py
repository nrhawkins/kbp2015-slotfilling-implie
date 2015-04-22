import string

# Writes all single word terms that are contained in another term that is not itself.
# This should catch most terms that are too general.
# Such as "university"
def main():
    inf = open('../jobTitle.txt', 'r')
    onef = open('raw_one_jobTitle.txt', 'r')

    out = open('jobTitle_wordcompout.txt', 'w')
    inl = inf.read().splitlines()
    onel = onef.read().splitlines()
    for one in onel:
        written = False
        for i in inl:
            if (not written and one.lower() in i.lower().split() and i != one):
                out.write(one + '\n')
                written = True
    out.close()



if __name__ == "__main__":
    main()
