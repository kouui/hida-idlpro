

;; comment : 
;; smenu function from
;; /home/ichimoto/idlpro/UTIL/widget/smenu.pro
;; imgsize procedure from
;; /home/ichimoto/idlpro/UTIL/imgsize.pro

;; history
;; 2022.04.21  k.u.  create to qlmap all dstpol data folders
;; 2022.04.21  k.u.  added wscan option to ku_qlmapall
;; 2022.04.26  k.u.  added axe to image; disable wlident; 
;;                   reverse file reading order to put the first data on the top
;;                   (S4 scan : from top to bottom)
;;                   added log scale conversion
;;                   profile axe position calculated in device coordinate
;; 2022.05.12  k.u.  migrated from lib.dstpol.qlmapall.pro;
;;                   dialog_filepick; spectra type; OS detect
;;----------------------------------------------------------------
;; TODO:
;; 1. is it necessary to combine qlmap and qlpol?
;;----------------------------------------------------------------

@import.dstpol.qlmap
;;----------------------------------------------------------------



FUNCTION __generate_menu, rootdir

folders0 = file_search(rootdir+'/*', /TEST_DIRECTORY)
folders = []
for i=0, n_elements(folders0)-1 do begin
	fdname = file_basename(folders0[i])
	;; skip folder of calibration data
	if strupcase(strmid(fdname,0,3)) eq 'CAL' then continue
	folders = [folders,[fdname]]
endfor
print, "[QLMAP] found data folders : ", folders

menu = ['quit','refresh',folders]

return, menu

END


;;----------------------------------------------------------------
;; select folder and then start quick look
;;----------------------------------------------------------------

PRO qlmap_folder, rootdir, wid=wid, wscan=wscan, ilogopt=ilogopt, kind=kind, status=status
; kind : vertical-single, vertical-dual, horizontal-single, horizontal-dual
status = 0b
_vh = strmid(kind,0,1)
_sd = strsplit(kind,'-', /extract) & _sd=_sd[1] &_sd = strmid(_sd,0,1)

print, '[QLMAP] _vh = ', strtrim(_vh,1), '  _sd = ',strtrim(_sd,1)
print, '[QLMAP] rootdir : ', rootdir
menu = __generate_menu(rootdir)
if n_elements(menu) eq 1 then begin
    print, '[QLMAP] ERROR: 0 folder founds.'
    status=1b
    return
endif

color_red = '9900ff'x

if not keyword_set(wid) then wid=7
while 1b do begin
    choice = smenu(menu,xpos=200,ypos=500,title='QLMAP FOLDERS')
    if choice eq 0 then break
    if choice eq 1 then begin
        menu = __generate_menu(rootdir)
        continue
    endif
    folder = menu[choice]
    ffind = rootdir + '/' + folder + '/*.fits'
    files = findfile(ffind)
    nf = n_elements(files)
    print, '[QLMAP] ', strtrim(nf,1), " .fits files found in folder : ", folder

    sp1 = readfits(files[0],h,nslice=0)
    _ss = size(sp1) & nx=_ss[1] & ny=_ss[2]

    nstep = nf
    case _vh of 
        'v' : np = nx
        'h' : np = ny
    endcase
    sps = fltarr(nx,ny,nstep)
    limg = fltarr(np,nstep)  ; [not used]
    j0 = 0

    ;; qlmap with wscan
    if keyword_set(wscan) then begin
        dd = 10
        wxs = 400
        wys = 600
        wxim = wxs-4*dd
        wyim = wxs-dd
        tvsclpos = [3*dd,wys-wxs]
        plotpos = [dd,3*dd,wxs-dd/2,wys-wxs-dd]
        axepos = [float(3*dd)/wxs,float(wys-wxs)/wys,float(wxs-dd)/wxs,float(wys-dd)/wys]
        for jj=0,nstep-1 do begin
            j = nstep-1-jj
            print,j,'  ',files[j0+j]
            sp1 = readfits(files[j0+j],h,nslice=0)
            sps[*,*,jj] = sp1[*,*]
        endfor
        mwidth = 5
        case _vh of
            'v' : begin
                _pos0 = nx/2
                if _sd eq 'd' then _pos0 /= 2
                prof = reform(rebin(sps[_pos0-mwidth:_pos0+mwidth,*,nstep/2],1,ny,1))
                wl = indgen(ny)

                _pos1 = nx-1
                if _sd eq 'd' then _pos1 /= 2
                im = sps[0:_pos1,ny/2,*]
            end
            'h' : begin
                _pos0 = ny/2
                if _sd eq 'd' then _pos0 /= 2
                prof = reform(rebin(sps[*,_pos0-mwidth:_pos0+mwidth,nstep/2],nx,1,1))
                wl = indgen(nx)

                _pos1 = ny-1
                if _sd eq 'd' then _pos1 /= 2
                im = sps[nx/2,0:_pos1,*]            
            end
        endcase
        im = congrid(reform(im),wxim,wyim)

        yr = [0.995, 1.005] * minmax(prof)
        xr = minmax(wl)
        window, wid, xs=wxs, ys=wys, xpos=600, ypos=200
        plot, [0,np],[0,nstep],yr=[nstep,0],xstyle=1,ystyle=1,/nodata,position=axepos,yticklen=-0.014,ytickformat='(I3)',xtickformat='(A1)'
        ;axis, tvsclpos[0], tvsclpos[1],yaxis=0, yticklen=-0.5, yrange=[0, nstep], color=color_red, /nodata,ystyle=1,/device
        tvscl, im, tvsclpos[0], tvsclpos[1]
        ;plot, wl, prof, yr=yr, xstyle=1, position=[0.03,0.05,0.98,(wys-wxs-2*dd)/float(wys)], /noerase,ytickformat='(A1)',xtickformat='(F7.0)'
        plot, wl, prof, yr=yr, xstyle=1, /device, position=plotpos, /noerase,ytickformat='(A1)',xtickformat='(F7.0)'
        
        islog = 0b
        last_button = 0
        while 1b do begin
            ;cursor, x, y, /data, /change
            cursor, x, y, /device, /change
            ;print, !mouse

            ;; left click to switch scale and right click to quit
            if keyword_set(ilogopt) then begin 
                ; mouse left click inside the image to change image scale
                if (!mouse.button eq 1) and $
                    ((x-tvsclpos[0])*(x-tvsclpos[0]-wxim) lt 0) and $
                    ((y-tvsclpos[1])*(y-tvsclpos[1]-wyim) lt 0) and $
                    (last_button ne 1) then begin
                        islog=~islog
                        if islog then scale = 'log' else scale = 'linear'
                        print, '[QLMAP] image scale -> '+scale+' scale'
                endif
                last_button = !mouse.button
                ; mouse right clicked to quit
                if !mouse.button eq 4 then begin 
                    wait, 0.5
                    break
                endif
            ;; quit with any click
            endif else begin
                if !mouse.button ne 0 then begin 
                    wait, 0.1
                    break
                endif
            endelse
            ;if (x lt xr[0]) or (x gt xr[1]) then continue
            ;if (y lt yr[0]) or (y gt yr[1]) then continue
            ;print, "x,y = ", x, y
            
            if (x lt plotpos[0]) or (x gt plotpos[2]) then continue
            if (y lt plotpos[1]) or (y gt plotpos[3]) then continue
            
            ;; move cursor inside profile axe to shift wavelength
            x=fix( xr[0]+(x-plotpos[0])*float(xr[1]-xr[0]+1)/(plotpos[2]-plotpos[0]+1) )
            _tmp = min(abs(wl-x), xpos)
            ;if dinfo.wl_order eq 1 then xpos = ny-1-xpos
            case _vh of
                'v' : im = sps[0:_pos1,xpos,*]
                'h' : im = sps[xpos,0:_pos1,*]
            endcase
            im = congrid(reform(im),wxim,wyim)
            if islog then im = alog(im)
            tvscl, im, tvsclpos[0], tvsclpos[1]

        endwhile
        wdelete, wid
    ;; qlmap without wscan [not used]
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

;;----------------------------------------------------------------
;; select root folder and spectra type
;;----------------------------------------------------------------

pro qlmap_main

;rootdir = '/mnt/HDD3TBn52/DST/sp/20220420/spec/He_I_10830/'
_test = findfile("C:\Pro*")
if keyword_set(_test) then _os='Windows' else _os='Linux'
print, '[QLMAP] OS : ', _os
case _os of
    'Windows' : initpath = 'D:\data'
    'Linux'   : initpath = '/mnt/'
endcase
rootdir = dialog_pickfile(path=initpath, title='SELECT ROOT FOLDER',/DIRECTORY)

menu = ['quit', $ 
        'vertical-single', $
        'vertical-dual', $
        'horizontal-single', $
        'horizontal-dual']
while 1b do begin
    choice = smenu(menu,xpos=200,ypos=500,title='QLMAP SPECTRA')
    if choice eq 0 then break
    kind = menu[choice]
    qlmap_folder, rootdir, /wscan, kind=kind, status=status
    if status eq 1b then break
endwhile
END
