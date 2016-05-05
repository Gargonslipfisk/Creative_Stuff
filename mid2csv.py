import re

f2 = open("Z.txt", "w")
with open("Pre.txt", "r") as f:
    data = f.readlines()
    for line in data:
        match = re.sub(r"\d, \d+, Note_off_c, 0, \d+, \d+", " ", line)
        f2.write(match)    
    else:
        f2.close()

^(?!.*note).*$

with open("Z.txt", "r+") as z:
    data2 = z.readlines()
    for line in data2:
        match2 = re.sub(",", ";", line)
        z.write(match2)
    else:
        z.close()

with open("Z.txt", "r") as Z:
    lines = Z.readlines()
    lines = filter(lambda x: not x.isspace(), lines)
    Z = open("Z.txt", "w")
    Z.write("".join(lines))
Z.close()

'''        
Z = open("Z.txt", "r+")
lines = Z.readlines()
Z.close()
lines = filter(lambda x: not x.isspace(), lines)
Z = open("Z.txt", "r+")
Z.write("".join(lines))
Z.close()
'''



       # print(match)

'''       
f2 = open("Z.txt", "w")
f2.write(match)
f.close()
f2.close()
'''
        #f.close()

'''
filename = "A.txt"
char1 = '[\d], [\d]+, Note_off_c, 0, [\d]+, [\d]+'
char2 = '[\n]'

filer = open(filename, "r")
filew = open(filename+'2', "w")

buff = filer.read()
rbuff = buff.replace(char1, char2)

filew.write(rbuff)

filer.close()
filew.close()
'''
'''
with open("A.txt", "r+") as f:
    data = f.readlines()
    for line in data:
        words = line.split()
        print(words)
        for word in words:
            print(word)
'''

# searchObj = re.search(pattern, string, flags=0)


      #	f.write("Hello World\n" "y otra linea")  

'''
file = open("A.txt", "r+")

file.write("hello world in the new file\n")

file.write("and another line\n")

file.close()
'''
