;; comment : 
;; smenu function from
;; /home/ichimoto/idlpro/UTIL/widget/smenu.pro
;; imgsize procedure from
;; /home/ichimoto/idlpro/UTIL/imgsize.pro

;; history
;; 2022.04.21  k.u.  create to qlmap all dstpol data folders
;; 2022.04.21  k.u.  added wscan option to ku_qlmapall
;; 2022.04.26  k.u   added axe to image; disable wlident

;;----------------------------------------------------------------
FUNCTION ku_generate_menu, rootdir

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

return, menu

END


;;----------------------------------------------------------------

PRO ku_qlmapAll, rootdir, file_ap, wid=wid, wscan=wscan, dinfo=dinfo, fitwl=fitwl

if keyword_set(wscan) and (not keyword_set(dinfo)) then begin
    throw_error, "in case of using wscan keyword, please provide dinfo"
    return
endif
menu = ku_generate_menu(rootdir)

print,'make QL map,  restoring ',file_ap
restore,file_ap	; -> ap

color_red = '9900ff'x

if not keyword_set(wid) then wid=7
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

    ;if dinfo.nstep eq -1 then nstep = nf else nstep = dinfo.nstep
    nstep = nf
    nxp = ap.ix2-ap.ix1+1
    sps = fltarr(nxp,ny,nstep)
    limg = fltarr(nxp,nstep)
    ;j0 = nstep*dinfo.i_scan
    j0 = 0

    if keyword_set(wscan) then begin
        dd = 10
        wxs = 400
        wys = 600
        wxim = wxs-4*dd
        wyim = wxs-dd
        tvsclpos = [3*dd,wys-wxs]
        axepos = [float(3*dd)/wxs,float(wys-wxs)/wys,float(wxs-dd)/wxs,float(wys-dd)/wys]
        for jj=0,nstep-1 do begin
            j = nstep-1-jj
            print,j,'  ',files[j0+j]
            sp1 = readfits(files[j0+j],h,nslice=1)
            sps[*,*,j] = sp1[ap.ix1:ap.ix2,*]
        endfor
        im = reverse(congrid(reform(sps[*,250,*]),wxim,wyim),2)
        prof = reform(rebin(sps[nxp/2-5:nxp/2+5,*,nstep/2],1,ny,1))
        if dinfo.wl_order eq 1 then prof=reverse(prof)
        if not keyword_set(wl) then begin
            if keyword_set(fitwl) then begin
            wl = wlident(prof, dinfo.wl0, dinfo.wl_range)
            endif else begin
            wl = indgen(ny)
            endelse
            ;if dinfo.wl_order eq 1 then wl=reverse(wl)
            ;wdelete, 0
        endif
        yr = [0.995, 1.005] * minmax(prof)
        xr = minmax(wl);[0,ny-1]
        window, wid, xs=wxs, ys=wys, xpos=600, ypos=200
        plot, [0,nxp],[0,nstep],yr=[nstep,0],xstyle=1,ystyle=1,/nodata,position=axepos,yticklen=-0.014,ytickformat='(I3)',xtickformat='(A1)'
        ;axis, tvsclpos[0], tvsclpos[1],yaxis=0, yticklen=-0.5, yrange=[0, nstep], color=color_red, /nodata,ystyle=1,/device
        tvscl, im, tvsclpos[0], tvsclpos[1]
        plot, wl, prof, yr=yr, xstyle=1, position=[0.03,0.05,0.98,(wys-wxs-2*dd)/float(wys)], /noerase,ytickformat='(A1)',xtickformat='(F7.0)'
        
        while 1b do begin
            cursor, x, y, /data, /change
            if !mouse.button ne 0 then break ; mouse clicked
            ;print, x, y
            if (x lt xr[0]) or (x gt xr[1]) then continue
            if (y lt yr[0]) or (y gt yr[1]) then continue
            ;print, "x,y = ", x, y
            ;xpos = ny-1-fix(x)
            _tmp = min(abs(wl-x), xpos)
            ;print, xpos
            if dinfo.wl_order eq 1 then xpos = ny-1-xpos
            im = reverse(congrid(reform(sps[*,xpos,*]),wxim,wyim),2)
            tvscl, im, tvsclpos[0], tvsclpos[1]
        endwhile
        wdelete, wid
    endif else begin
        window,wid,xs=nx,ys=ny
        tvscl,sp1
        print,'click spectral position on left sp'
        cursor,px,py,3,/dev

        for j=0,nstep-1 do begin
            print,j,'  ',files[j0+j]
            sp1 = readfits(files[j0+j],h,nslice=1)
            limg[*,j] = sp1[ap.ix1:ap.ix2,py]
        endfor
        tvf,reverse(congrid(limg,200,200),2)
        ;tvf,congrid(limg,200,200)
        wshow
    endelse
endwhile

END