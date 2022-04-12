

;        ['white'  , 'green'  , 'red'    , 'yellow' , 'purple' , 'blue'   , 'brown'  , 'cyan'   ]
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x, '393893'x, 'd67e40'x, '206b5c'x, 'edf580'x] ; 'bbggrr'x

date = '20220316'
identifier = 'ircamera-experiment'
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'+date+'.'+identifier+'/'
charsize=1.5

;gotolabel = 'make data'
;gotolabel = 'visual1'
;gotolabel = 'make data2'
;gotolabel = 'visual2'
gotolabel = 'checkflir'
case gotolabel of 
    'make data'   : goto, label_make_data
    'visual1'     : goto, label_visual1
    'make data2'  : goto, label_make_data2
    'visual2'     : goto, label_visual2
    'checkflir'   : goto, label_check_flir
endcase
goto, label_the_end


label_make_data:

version = '0316'
version = '0320'
cameras = ['flir','gold','xeva']

;x1 = 350 & x2 = 450
;y1 = 256-50 & y2 = 256+50
;x1 = 250 & x2 = x1
x1 = 280 & x2 = x1
y1 = 256-50 & y2 = 256+50

;;-- loop over camera
for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]

exps = ['0008','0015','0040','0100','0200','0500']
case camera of 
    'gold' : exps = [exps,'1000']
    else   : exps = ['0001','0002','0004',exps, '1000']
endcase

case version of 
    '0316' : begin
        root = '/mnt/HDD3TBn51/DST/vs/20220316/'
        folder = root + camera + '/'
    end
    '0320' : begin
        if (camera eq 'gold') or (camera eq 'flir') then begin
        root = '/mnt/HDD3TBn52/DST/vs/20220320/'
        folder = root + camera + '/'
        endif else begin
        root = '/mnt/HDD3TBn51/DST/vs/20220316/'
        folder = root + camera + '/'
        endelse
    end
endcase
print, 'processing folder : ', folder

curve = dblarr(n_elements(exps))
expv  = dblarr(n_elements(exps))
;;-- loop over exposure
for jj=0, n_elements(exps)-1 do begin
exp = exps[n_elements(exps)-1-jj]

fdatas = findfile(folder+'/exp6_'+exp+'_'+camera+'*.fits')
fdarks = findfile(folder+'/dark_e'+exp+'*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'
nf =  n_elements(fdatas)


print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2]
if ss[0] eq 3 then dark = reform(rebin(dark, nx, ny, 1))

print, 'calculating sn data ...'

img = float(readfits(fdatas[0],hd, /silent))
ss = size(img) & nx = ss[1] & ny = ss[2] & nt = ss[3]
img = mean(img, dimension=3, /double)
;for i=0,nt-1 do img[*,*,i] = img[*,*,i] - dark
img = img - dark
;stop

expv[jj] = float(exp)
curve[jj] = rebin(img[x1:x2,y1:y2],1,1)
    

endfor ;;-- end loop over exposure

if 1b then begin
res = hash()
res['expv'] = expv
res['curve'] = curve
fname = savedir+'/linear/'+camera+'.linear.v'+version+'.sav'
save, res, filename=fname
print, 'saved as : ', fname
endif

endfor ;;-- end loop over camera

stop

label_visual1:

_IS_SAVE = 1b

version = '0316'
version = '0320'
cameras = ['flir','gold','xeva']
psym = 4


;;-- loop over camera

window, 7, xs=800, ys=600
for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]

;exps = ['0008','0015','0040','0100','0200','0500']
;case camera of 
;    'gold' : exps = [exps,'1000']
;    else   : exps = ['0001','0002','0004',exps]
;endcase

fname = savedir+'/linear/'+camera+'.linear.v'+version+'.sav'
restore, fname
print, 'restored : ', fname
expv = res['expv']
if camera eq 'gold' then expv -= 3.32;4.5
curve = res['curve']

if kk eq 0 then begin
plot, expv, curve, /xlog, /ylog,  psym=psym, color=colors[kk], xtitle='exposure [ms]', ytitle='mean value', charsize=charsize, symsize=4, yr=[10, 20000];,xr=[0,110]
oplot, expv, curve, color=colors[kk]
endif else begin
oplot, expv, curve, psym=psym, color=colors[kk], symsize=4
oplot, expv, curve, color=colors[kk]
endelse

endfor ;;-- end loop over camera

oplot, [1,1000], [1,1000]*10, line=2


fname = savedir + '/linear/' + 'cameras.linear.v'+version+'.png'
if _IS_SAVE then begin
WRITE_PNG, fname, TVRD(/TRUE)
print, 'saved as : ', fname
endif

stop

label_make_data2:


version = '0316'
version = '0320'
cameras = ['flir','gold','xeva']

;x1 = 350 & x2 = 450
;y1 = 256-50 & y2 = 256+50
;x1 = 250 & x2 = x1
x1 = 280 & x2 = x1
y1 = 256-50 & y2 = 256+50

;;-- loop over camera
for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]

exps = ['0008','0015','0040','0100','0200','0500']
case camera of 
    'gold' : exps = [exps,'1000']
    else   : exps = ['0001','0002','0004',exps, '1000']
endcase

case version of 
    '0316' : begin
        root = '/mnt/HDD3TBn51/DST/vs/20220316/'
        folder = root + camera + '/'
    end
    '0320' : begin
        if (camera eq 'gold') or (camera eq 'flir') then begin
        root = '/mnt/HDD3TBn52/DST/vs/20220320/'
        folder = root + camera + '/'
        endif else begin
        root = '/mnt/HDD3TBn51/DST/vs/20220316/'
        folder = root + camera + '/'
        endelse
    end
endcase
print, 'processing folder : ', folder

;;-- loop over exposure
for jj=0, n_elements(exps)-1 do begin
exp = exps[n_elements(exps)-1-jj]

fdatas = findfile(folder+'/exp6_'+exp+'_'+camera+'*.fits')
fdarks = findfile(folder+'/dark_e'+exp+'*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'
nf =  n_elements(fdatas)


print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2]
if ss[0] eq 3 then dark = reform(rebin(dark, nx, ny, 1))

print, 'calculating sn data ...'

img = float(readfits(fdatas[0],hd, /silent))
ss = size(img) & nx = ss[1] & ny = ss[2] & nt = ss[3]
img = mean(img, dimension=3, /double)
;for i=0,nt-1 do img[*,*,i] = img[*,*,i] - dark
img = img - dark
;stop

if 1b then begin
res = hash()
res['img'] = img
fname = savedir+'/linear/'+camera+'.e'+exp+'.vs40.v'+version+'.sav'
save, res, filename=fname
print, 'saved as : ', fname
endif

endfor ;;-- end loop over exposure

endfor ;;-- end loop over camera

stop

label_visual2:

_IS_SAVE = 1b

version = '0320'

cameras = ['flir','gold','xeva']
psym = 3

exps = ['0008','0015','0100','0200','0500', '1000']
exp0 = '0040'


;; start loop over cameras
for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]

exp = exp0
fname = savedir+'/linear/'+camera+'.e'+exp+'.vs40.v'+version+'.sav'
restore, fname
print, 'restored : ', fname
img40 = res['img']
ss = size(img40) & nx = ss[1] & ny = ss[2]
img40 = reform(img40, nx*ny)

window, kk+3, xs=500, ys=600
;; start loop over exposure
for jj=0, n_elements(exps)-1 do begin
exp = exps[jj]
fname = savedir+'/linear/'+camera+'.e'+exp+'.vs40.v'+version+'.sav'
restore, fname
print, 'restored : ', fname
img = reform(res['img'], nx*ny)
if camera eq 'gold' then exp -= 3.32
img = img / float(exp) * 1000.

if jj eq 0 then begin
plot, img40, img,  psym=psym,symsize=1, color=colors[jj], xtitle='Count [DN], exposure=40[ms]', ytitle='Count/exposure [DN/s]',title=camera, charsize=charsize, yr=[0, 100000], xr=[-2000,3000]
if camera eq 'flir' then oplot, [0,3000], [1E4, 82000], color=colors[0]
if camera eq 'gold' then oplot, [0,3000], [1.5E4, 94000], color=colors[0]
endif else begin
oplot,img40, img,  psym=psym,symsize=1, color=colors[jj]
endelse

endfor ;; end loop over exposure

if _IS_SAVE then begin
fname = savedir+'/linear/'+camera+'.vs40.v'+version+'.png'
WRITE_PNG, fname, TVRD(/TRUE)
print, 'saved as : ', fname
endif

endfor ;; end loop over cameras

stop

label_check_flir:

_IS_SAVE = 1b
_IS_CHECK_THRESHOLD = 1b

version = '0320'

;cameras = ['flir','gold','xeva']
psym = 3

exps = ['0008','0015','0100','0200','0500', '1000']
exp0 = '0040'

camera = 'flir'

exp = exp0
fname = savedir+'/linear/'+camera+'.e'+exp+'.vs40.v'+version+'.sav'
restore, fname
print, 'restored : ', fname
img40 = res['img']
ss = size(img40) & nx = ss[1] & ny = ss[2]
img40 = reform(img40, nx*ny)

; threshold
coe = [1.4E4,24]
x = [-1000, 3000]

if _IS_CHECK_THRESHOLD then begin
window, 3, xs=500, ys=600
;; start loop over exposure
for jj=0, n_elements(exps)-1 do begin
exp = exps[jj]
fname = savedir+'/linear/'+camera+'.e'+exp+'.vs40.v'+version+'.sav'
restore, fname
print, 'restored : ', fname
img = reform(res['img'], nx*ny)
if camera eq 'gold' then exp -= 3.32
img = img / float(exp) * 1000.

if jj eq 0 then begin
plot, img40, img,  psym=psym,symsize=1, color=colors[jj], xtitle='Count [DN], exposure=40[ms]', ytitle='Count/exposure [DN/s]',title=camera, charsize=charsize, yr=[0, 100000], xr=[-2000,3000]

oplot, x, x*coe[1]+coe[0], color=colors[0]
endif else begin
oplot,img40, img,  psym=psym,symsize=1, color=colors[jj]
endelse

endfor ;; end loop over exposure
if _IS_SAVE then begin
fname = savedir+'/linear/'+camera+'.vs40.abnormal.threshold.v'+version+'.png'
WRITE_PNG, fname, TVRD(/TRUE)
print, 'saved as : ', fname
endif

endif

dd = 0
window, 4, xs=nx*2+dd, ys=ny
;; start loop over exposure
for jj=0, n_elements(exps)-1 do begin
exp = exps[jj]
case version of 
    '0316' : begin
        root = '/mnt/HDD3TBn51/DST/vs/20220316/'
        folder = root + camera + '/'
    end
    '0320' : begin
        if (camera eq 'gold') or (camera eq 'flir') then begin
        root = '/mnt/HDD3TBn52/DST/vs/20220320/'
        folder = root + camera + '/'
        endif else begin
        root = '/mnt/HDD3TBn51/DST/vs/20220316/'
        folder = root + camera + '/'
        endelse
    end
endcase
print, 'processing folder : ', folder

fdatas = findfile(folder+'/exp6_'+exp+'_'+camera+'*.fits')
fdarks = findfile(folder+'/dark_e'+exp+'*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'
nf =  n_elements(fdatas)


print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2]
if ss[0] eq 3 then dark = reform(rebin(dark, nx, ny, 1))

print, 'calculating sn data ...'

img = float(readfits(fdatas[0],hd, /silent))
ss = size(img) & nx = ss[1] & ny = ss[2] & nt = ss[3]
for i=0,nt-1 do img[*,*,i] = img[*,*,i] - dark


;; start loop over nimg
for i=0, nt-1 do begin
    img1 = img[*,*,i]
    tvscl, img1, 0, 0
    img1 = reform(img1,nx*ny)
    y = img1/ float(exp) * 1000.
    pos = where( y gt img40*coe[1]+coe[0], count)
    print, "# of abnormal pixels : ", count
    img1[*] = 0
    img1[pos] = 1
    img1 = reform(img1, nx, ny)
    tvscl, img1, nx+dd, 0
    if _IS_SAVE then begin
    fname = savedir+'/linear/'+camera+'.vs40.abnormal.e'+exp+'.nimg'+strtrim(i,2)+'.v'+version+'.png'
    WRITE_PNG, fname, TVRD(/TRUE)
    print, 'saved as : ', fname
    endif
endfor ;; end loop over nimg

endfor ;; end loop over exposure


stop

label_the_end:
END