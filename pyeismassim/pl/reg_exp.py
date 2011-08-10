import re
with open ('kmap.pl') as f:
    for line in f:
        # print re.sub(r'k(\w*)\(([\w\s\d,]*)\)', r'k(\1(\2))', line[:-1])
        print re.sub(r'k\(edge\(([\w\s\d]*),\s*([\w\s\d]*),\s*([\w\s\d]*)\)', r'k(edge(\2, \1, \3))', line[:-1])
        