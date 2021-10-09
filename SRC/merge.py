import sys

with open(sys.argv[1], 'r') as original:
    origin = original.readlines()

with open('slush', 'r') as ptfs:
    ptf = ptfs.readlines()

d = {}

for l in origin:
    d[l.rstrip()[-8:]] = l
for l in ptf:
    d[l.rstrip()[-8:]] = l

for key in sorted(d):
    print (d[key].rstrip())