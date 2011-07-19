import re
with open ('kmap.pl') as f:
    for line in f:
        print re.sub(r'k(\w*)\(([\w\s\d,]*)\)', r'k(\1(\2))', line[:-1])
        