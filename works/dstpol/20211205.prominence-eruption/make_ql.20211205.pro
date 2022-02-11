
@dst_file_utils

;;------ path and parameters
_PATH = "/mnt/HDD3TBn50/DST/sp/20211205/slitjaw/Ha"
_PATH_SPEC = "/mnt/HDD3TBn50/DST/sp/20211205/spec/HeI_10830"
_SAVE_PATH = "/nwork/kouui/data-lvl1/dstpol/20211205.priminence-eruption/slitjaw"
_SAVE_DARK = _SAVE_PATH + "/calib.dark.sav"
_SAVE_FLAT = _SAVE_PATH + "/calib.flat.sav"
_SAVE_SCAN_INDICATOR = _SAVE_PATH + "/calib.scan_indicator.sav"
_IMAGE_SIZE_X = 2048
_IMAGE_SIZE_Y = 2048
_WIN_REBIN_FACTOR = 4
_WIN_SIZE_X = _IMAGE_SIZE_X / _WIN_REBIN_FACTOR
_WIN_SIZE_Y = _IMAGE_SIZE_Y / _WIN_REBIN_FACTOR
_PREFIXS = ["pr_","pr2_","pr3_","pr6_","pr8_","pr9_", "NONE"]

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

files = FILE_SEARCH(_PATH + "/calib/dark_*.fits", count=nf)
PRINT, nf, " dark files found!"

dark3d = READFITS(files[2], hd)
dark3d_size = SIZE(dark3d)
if (dark3d_size[0] ne 3) then THROW_ERROR, "dark3d has ndim!=3"

dark = MEAN(dark3d, DIMENSION=_AXIS_TIME, /DOUBLE)
; delete unnecessary variables
UNDEFINE, dark3d
UNDEFINE, dark3d_size
UNDEFINE, files
UNDEFINE, nf

; take a look at dark image
if (0) then begin
WINDOW, 1, xsize=_WIN_SIZE_X, ysize=_WIN_SIZE_Y
tvscl, rebin(dark, _WIN_SIZE_X, _WIN_SIZE_Y)
endif
; save dark array
SAVE, filename=_SAVE_DARK, dark 
stop

;;----- make flat
_case_flat:

files = FILE_SEARCH(_PATH + "/calib/flat_*.fits", count=nf)
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

if (0) then begin
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
RESTORE, _SAVE_SCAN_INDICATOR

files = FILE_SEARCH(_PATH + "/scan/*.fits", count=nf)
PRINT, nf, " image files found!"

img = READFITS(files[0], hd)
img_size = SIZE(img)
if (img_size[0] ne 2) then THROW_ERROR, "flat has ndim!=2"

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
