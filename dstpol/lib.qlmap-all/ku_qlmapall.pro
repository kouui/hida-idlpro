
;; smenu function from
;; /home/ichimoto/idlpro/UTIL/widget/smenu.pro
;; imgsize procedure from
;; /home/ichimoto/idlpro/UTIL/imgsize.pro

;; history
;; 2022.04.21  k.u.  create for qlmap all dstpol data folders

PRO ku_qlmapAll, rootdir, file_ap, wid=wid

if not keyword_set(wid) then wid=7

folders0 = file_search(rootdir+'/*', /TEST_DIRECTORY)
folders = []
for i=0, n_elements(folders0)-1 do begin
	fdname = file_basename(folders0[i])
	;; skip folder of calibration data
	if strmid(fdname,0,3) eq 'cal' then continue
	folders = [folders,[fdname]]
endfor
print, "found data folders ", folders

menu = ['quit',folders]

print,'make QL map,  restoring ',file_ap
restore,file_ap	; -> ap

while 1b do begin
    choice = smenu(menu,xpos=200,ypos=500,title='QLMAP ALL')
    if choice eq 0 then break
    folder = menu[choice]
    ffind = rootdir + '/' + folder + '/*.fits'
    files = findfile(ffind)
    nf = n_elements(files)
    print, nf, " .fits files found in folder : ", folder

    sp1 = readfits(files[0],h,nslice=1)
    imgsize,sp1,nx,ny

    window,wid,xs=nx,ys=ny
    tvscl,sp1
    print,'click spectral position on left sp'
    cursor,px,py,3,/dev

    ;if dinfo.nstep eq -1 then nstep = nf else nstep = dinfo.nstep
    nstep = nf
    sps = fltarr(nx,ny,nstep)
    nxp = ap.ix2-ap.ix1+1
    limg = fltarr(nxp,nstep)
    ;j0 = nstep*dinfo.i_scan
    j0 = 0

    for j=0,nstep-1 do begin
        print,j,'  ',files[j0+j]
        sp1 = readfits(files[j0+j],h,nslice=1)
        limg[*,j] = sp1[ap.ix1:ap.ix2,py]
    endfor
    tvf,reverse(congrid(limg,200,200),2)
    ;tvf,congrid(limg,200,200)
    wshow
end

END