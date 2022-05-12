
datadir = '/mnt/HDD3TBn52/DST/sp/20220420/spec/He_I_10830/calib/'
outdir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/20220420.xclass-ar/flatiquv/'
outfile = outdir + 'flat_mean.20220509.fits'
if not file_test(outdir) then file_mkdir, outdir

fdatas = findfile(datadir+'/flat*.fits')
nf = n_elements(fdatas)
for i=0,nf-1 do begin
	img    = readfits(fdatas[i],hdim,exten_no=0)
	if i eq 0 then begin
		dstarr  = readfits(fdatas[i],hdda,exten_no=1)
		timarr  = readfits(fdatas[i],hdti,exten_no=2)
		imgs    = float(img)	
	endif	else imgs = imgs + float(img)
endfor
imgs /= nf

mwrfits,imgs,outfile,hdim
mwrfits,dstarr,outfile,hdda
mwrfits,timarr,outfile,hdti

END
