
@dst_file_utils

;;------ path and parameters
_YMD = "20220110"
_FIX_YMD = 1b
_PLOT_PREFIX = 0b
_PATH = "/mnt/HDD3TBn51/DST/sp/20220110/slitjaw/Ha"
_PATH_SPEC = "/mnt/HDD3TBn51/DST/sp/20220110/spec/HeI_10830"
_SAVE_PATH = "/nwork/kouui/data-lvl1/dstpol/20220110.giant-prominence/slitjaw"
_SAVE_DARK = _SAVE_PATH + "/calib.dark.sav"
_SAVE_FLAT = _SAVE_PATH + "/calib.flat.sav"
_SAVE_SCAN_INDICATOR = _SAVE_PATH + "/calib.scan_indicator.sav"
_IMAGE_SIZE_X = 2048
_IMAGE_SIZE_Y = 2048
_WIN_REBIN_FACTOR = 4
_WIN_SIZE_X = _IMAGE_SIZE_X / _WIN_REBIN_FACTOR
_WIN_SIZE_Y = _IMAGE_SIZE_Y / _WIN_REBIN_FACTOR
;;_PREFIXS = ["pr_","pr2_","pr3_","pr6_","pr8_","pr9_", "NONE"]
_SPEC_FOLDERS = ["ar1", "ar2", "ar3", "pr1", "pr2"]
_PREFIXS = [_SPEC_FOLDERS, "NONE"]

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

dark3d = READFITS(files[0], hd)
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

;files = FILE_SEARCH(_PATH_SPEC + "/scan/pr*.fits", count=nf)
files = []
nfolder = n_elements(_SPEC_FOLDERS)
for i=0,nfolder-1 do begin
    files1 = FILE_SEARCH(_PATH_SPEC + "/" + _SPEC_FOLDERS[i] + "/flir*.fits", count=nf)
    print, "[seq] ", _SPEC_FOLDERS[i], " : found ", nf, " files"
    for j=0,nf-1 do files1[j] = REPSTR(files1[j],"/flir","/"+_SPEC_FOLDERS[i]+"-flir")
    ;;print, files1[1]
    files = [files, files1]
endfor
print, "totally ", n_elements(files), " spec fits files found."
ranges = dst_scan_seconds_range(files, _PREFIXS, -10)
files = FILE_SEARCH(_PATH + "/scan/*.fits", count=nf)
indicators = dst_scan_indicator(files, _PREFIXS, ranges)

if (0) then begin
    for i=0,nf-1 do print, _PREFIXS[indicators[i]], " ",  path_basename(files[i])
endif

UNDEFINE, files1
UNDEFINE, nfolder
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
count = 0
WINDOW, 1, xsize=_WIN_SIZE_X, ysize=_WIN_SIZE_Y
is_log = 0b
for i=3354,nf-1 do begin
    pre = _PREFIXS[indicators[i]]
    ;;if (pre eq "NONE") then continue
    ;if (~(pre eq "pr9_")) then continue
    time_str = dst_time2str(dst_filename2time(files[i]))
    if (_FIX_YMD) then begin
    words = STRSPLIT(time_str, ' ', /EXTRACT)
    time_str = _YMD+" "+words[1]
    endif
    if (_PLOT_PREFIX) then time_str = time_str + " " + pre
    img = READFITS(files[i], hd)
    img = (double(img)-dark[*,*]); / flat[*,*]
    ;img = ALOG10(img < 1 > 32000)
    ;if (str_startswith(pre, "pr")) then is_log=1b
    is_log=1b
    if (is_log) then img = ALOG10(img > 10 < 16000) else img = img > 10 < 16000
    TVSCL, rebin(img, _WIN_SIZE_X, _WIN_SIZE_Y);, TOP=100
    ;print, min(img), max(img)
    xyouts, 0.05,0.9,time_str, CHARSIZE=3, COLOR=!color.black
    filename = _SAVE_PATH+'/png/'+string(count,format='%05d')+"."+pre+".png"
    print, "saved as --> ", filename
    WRITE_PNG,filename,TVRD(/TRUE)
    ;;if (i eq 0) then break
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
