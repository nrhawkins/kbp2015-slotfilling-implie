import string

def main():
    inf = open('school.txt', 'r')
    onef = open('one_word_school.txt', 'r')

    out = open('wordcompout.txt', 'w')
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
