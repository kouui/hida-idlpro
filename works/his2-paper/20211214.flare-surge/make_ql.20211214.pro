
@dst_file_utils

;;------ path and parameters
_PATH = "/mnt/HDD3TBn50/DST/sp/20211214/slitjaw/"
;_PATH_SPEC = "/mnt/HDD3TBn50/DST/sp/20211214/spec/HeI_10830"
_SAVE_PATH = "/nwork/kouui/data-lvl1/dstpol/20211214.flare-surge/slitjaw"
_SAVE_DARK = _SAVE_PATH + "/calib.dark.sav"
_SAVE_FLAT = _SAVE_PATH + "/calib.flat.sav"
_SAVE_SCAN_INDICATOR = _SAVE_PATH + "/calib.scan_indicator.sav"
_IMAGE_SIZE_X = 2048
_IMAGE_SIZE_Y = 2048
_WIN_REBIN_FACTOR = 8
_WIN_SIZE_X = _IMAGE_SIZE_X / _WIN_REBIN_FACTOR
_WIN_SIZE_Y = _IMAGE_SIZE_Y / _WIN_REBIN_FACTOR
;_PREFIXS = ["pr_","pr2_","pr3_","pr6_","pr8_","pr9_", "NONE"]

_AXIS_TIME = 3

;;----- program flow control
_case_label = 4
case _case_label of
    1 : goto, _case_dark
    2 : goto, _case_flat
    3 : goto, _case_time
    4 : goto, _case_ql
endcase


;;----- make dark
_case_dark:

files = FILE_SEARCH(_PATH + "/calib/dark*.fits", count=nf)
PRINT, nf, " dark files found!"

dark = fltarr(_IMAGE_SIZE_X,_IMAGE_SIZE_Y)
for i=0,nf-1 do begin
    dark2d = READFITS(files[i], hd)
    dark[*,*] += dark2d
endfor
dark = dark / nf
;dark3d = READFITS(files[1], hd)
;dark3d_size = SIZE(dark3d)
;if (dark3d_size[0] ne 3) then THROW_ERROR, "dark3d has ndim!=3"
;dark = MEAN(dark3d, DIMENSION=_AXIS_TIME, /DOUBLE)
; delete unnecessary variables
UNDEFINE, dark2d
UNDEFINE, dark3d
UNDEFINE, dark3d_size
UNDEFINE, files
UNDEFINE, nf

; take a look at dark image
if (1) then begin
WINDOW, 1, xsize=_WIN_SIZE_X, ysize=_WIN_SIZE_Y
tvscl, rebin(dark, _WIN_SIZE_X, _WIN_SIZE_Y)
endif
; save dark array
SAVE, filename=_SAVE_DARK, dark 
stop

;;----- make flat
_case_flat:

files = FILE_SEARCH(_PATH + "/calib/flat*.fits", count=nf)
PRINT, nf, " flat files found!"

img = READFITS(files[0], hd)
img_size = SIZE(img)
if (img_size[0] ne 2) then THROW_ERROR, "flat has ndim!=2"


RESTORE, _SAVE_DARK
flat = dblarr(_IMAGE_SIZE_X,_IMAGE_SIZE_X)
for i=0,nf-1 do begin
    img = READFITS(files[i], hd)
    flat[*,*] += (img[*,*]-dark[*,*])
endfor
flat[*,*] /= nf
flat[*,*] /= MEAN(flat[*,*])

UNDEFINE, img
UNDEFINE, nf
UNDEFINE, img_size
UNDEFINE, files
UNDEFINE, dark

if (1) then begin
WINDOW, 1, xsize=_WIN_SIZE_X, ysize=_WIN_SIZE_Y
tvscl, rebin(flat, _WIN_SIZE_X, _WIN_SIZE_Y)
endif

SAVE, filename=_SAVE_FLAT, flat 
stop
;;----- seperate time series
_case_time:

files = FILE_SEARCH(_PATH_SPEC + "/scan/pr*.fits", count=nf)
ranges = dst_scan_seconds_range(files, _PREFIXS, -10)
files = FILE_SEARCH(_PATH + "/scan/*.fits", count=nf)
indicators = dst_scan_indicator(files, _PREFIXS, ranges)

if (0) then begin
    for i=0,nf-1 do print, _PREFIXS[indicators[i]], " ",  path_basename(files[i])
endif

UNDEFINE, files
UNDEFINE, nf
UNDEFINE, ranges

SAVE, filename=_SAVE_SCAN_INDICATOR, indicators
stop
;;----- make quick look png images
_case_ql:

RESTORE, _SAVE_DARK
RESTORE, _SAVE_FLAT
;RESTORE, _SAVE_SCAN_INDICATOR

files = FILE_SEARCH(_PATH + "/scan/*.fits", count=nf)
PRINT, nf, " image files found!"
;; dwl in hd is in chaos!!!

dd = 2
WINDOW, 0, xsize=_WIN_SIZE_X*4+dd*3, ysize=_WIN_SIZE_Y*2+dd
xpos = [3,2,1,0,0,1,2,3]*_WIN_SIZE_X
xpos += [3,2,1,0,0,1,2,3]*dd
ypos = [1,1,1,1,0,0,0,0]*_WIN_SIZE_Y
ypos += [1,1,1,1,0,0,0,0]*dd
label = ['-1.2A','-0.9A','-0.6A','-0.3A','+0.3A','+0.6A','+0.9A','+1.2A']
; 1117(k=0-14)
; 1123(k=20-33)
; 1125(k=35-43)
offset = 1125
;k = 100
for k =35,43 do begin
    for i=0,7 do begin
        ;if k eq 15 then offset += 6
        img = READFITS(files[offset+i+9*k], hd)
        img_size = SIZE(img)
        if (img_size[0] ne 2) then THROW_ERROR, "flat has ndim!=2"

        ;WINDOW, 0, xsize=_WIN_SIZE_X, ysize=_WIN_SIZE_Y
        img = (double(img)-dark); / flat
        TVSCL, rebin(img, _WIN_SIZE_X, _WIN_SIZE_Y), xpos[i],ypos[i]
        xyouts, xpos[i]+20,ypos[i]+_WIN_SIZE_Y-20,label[i], CHARSIZE=2, COLOR=!color.black, /device
        if i eq 3 then xyouts, xpos[i]+20,ypos[i]+_WIN_SIZE_Y-40,strmid(hd[6],10,20), CHARSIZE=1.5, COLOR=!color.black, /device
        ;print, hd
    endfor
    filename = _SAVE_PATH+'/png/'+string(k-(35-29),format='%05d')+".png"
    WRITE_PNG,filename,TVRD(/TRUE)
endfor
stop
WINDOW, 1, xsize=_WIN_SIZE_X, ysize=_WIN_SIZE_Y
count = 0
for i=0,nf-1 do begin
    pre = _PREFIXS[indicators[i]]
    if (pre eq "NONE") then continue
    ;if (~(pre eq "pr9_")) then continue
    time_str = dst_time2str(dst_filename2time(files[i]))
    img = READFITS(files[i], hd)
    img = (double(img)-dark[*,*]) / flat[*,*]
    ;img = ALOG10(img < 1 > 32000)
    img = ALOG10(img > 10 < 32000)
    TVSCL, rebin(img, _WIN_SIZE_X, _WIN_SIZE_Y);, TOP=100
    ;print, min(img), max(img)
    xyouts, 0.05,0.9,time_str, CHARSIZE=3, COLOR=!color.black
    filename = _SAVE_PATH+'/png/'+pre+'/'+string(count,format='%05d')+".png"
    print, "saved as --> ", filename
    WRITE_PNG,filename,TVRD(/TRUE)
    ;if (i eq 0) then break
    count += 1
endfor

UNDEFINE, files
UNDEFINE, nf
UNDEFINE, img
UNDEFINE, time_str
UNDEFINE, filename
UNDEFINE, dark
UNDEFINE, files
UNDEFINE, flat
UNDEFINE, files
UNDEFINE, indicators
UNDEFINE, count
stop
END
