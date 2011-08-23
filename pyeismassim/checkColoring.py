
for x in ['a1-kb.txt','a2-kb.txt','a3-kb.txt','b1-kb.txt','b2-kb.txt','b3-kb.txt']:
	i = 0
	print x
	with open(x) as f:
		node = False
		for line in f:
			if line.find('Node: ') == 0:
				# print line
				if node:
					print i, line
				node = True
			elif line.find('Points: ') ==0:
				# print line
				if not node:
					print i, line
				else:
					node = False
			i += 1
	print x