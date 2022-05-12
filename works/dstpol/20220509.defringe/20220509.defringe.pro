
@lib.dstpol.dispiquvr

rootdir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/20220420.xclass-ar/'
nfolder=2
folders=['ar.2.1/','ar.2.2/']

files0 = findfile(rootdir+folders[0]+'sav/*.sav')
files1 = findfile(rootdir+folders[1]+'sav/*.sav')

nsav=n_elements(files0)
bias= 0;V: 50; Q: 16
pmax=0.001
wid=1
for i=0,nsav-1-bias do begin
	restore, files0[i+bias]
	s0 = s
	restore, files1[i]
	s1 = s
	s = s0-s1
	if i eq 0 then ku_dispiquvr, s, bin=1, pmax=pmax, wid=1 $
	else ku_dispiquvr, s, bin=1, pmax=pmax, wid=1, /wexist 
	print, 'i=', i
	wait,0.1
endfor

END
