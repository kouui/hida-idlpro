#! /usr/bin/python

import os
import shutil

_NBAR = 50
_rootdir = os.path.dirname(__file__)
file = os.path.join(_rootdir, 'module.txt')

config = {'main':[], 'import':[], 'dependency':[]}

with open(file, 'r') as f:
    lines = [ln.strip() for ln in f]

for line in lines:

    ## skip empty line
    if len(line)==0: continue
    ## skip mistype lines
    if len(line) <2: continue
    ## skip comment line
    if line[0] == '#': continue

    ## get tag
    if line[0]=='-' and len(line)>1:
        tag = line[1:].strip()
        assert tag in config.keys(), 'undefined tag ' + tag
        continue
    
    if tag == 'import' and len(config[tag]) ==1:
        print('[Wanring] support 1 import file only')
        print('[Wanring] use: ',config[tag][0])
    config[tag].append(line) 

## log
print('-'*_NBAR)
for key, val in config.items():
    print("["+key+"]")
    for v in val:
        print(v)
print('-'*_NBAR)
print("[copy files]")
outdir = 'lib/'
try:  os.makedirs(outdir) 
except OSError: pass
depens = []
for key, val in config.items():
    if key == 'import' : continue
    for v in val:
        src = v
        tar = os.path.join( outdir, os.path.basename(src) )
        if key == 'main':
            words = tar.split('.')
            pos = -2
            ver = words[pos]
            if ver.isdigit and len(ver)==8:
                words.pop(pos)
                tar = '.'.join(words)        
        if key == 'dependency' : depens.append( os.path.basename(tar)  )
        shutil.copyfile(src, tar) 
        print(src)
        print('-->')
        print(tar)
print("[make import file]")
imfile = config['import'][0]
imfile = os.path.join( outdir, imfile )
print(imfile+' :')
with open(imfile, 'w') as f:
    for name in depens:
        content = '@'+name
        f.write(content)
        print(content)
print("[completed]")
print('-'*_NBAR)
