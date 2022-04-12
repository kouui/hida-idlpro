
;        ['white'  , 'green'  , 'red'    , 'yellow']
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x
;        [◇        , △        , □        , x        ]
psyms  = [4        , 5        , 6        , 7        ]

date = '20220316'
identifier = 'ircamera-experiment'
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'+date+'.'+identifier+'/'
charsize=1.5



;gotolabel = 'sn 140ms'
;gotolabel = 'scatter plot'
gotolabel = 'sn multi'
;gotolabel = 'scatter plot multi'
;gotolabel = 'check gold pixels'
;gotolabel = 'hist plot multi'
;gotolabel = 'sn multi check dark'

case gotolabel of 
    'sn 140ms'    : goto, label_sn140
    'scatter plot': goto, visual1
    'sn multi'    : goto, label_sn_multi
    'scatter plot multi' : goto, visual1_multi
    'check gold pixels' : goto, label_check_gold
    'hist plot multi' : goto, label_hist_plot_multi
    'sn multi check dark'    : goto, label_sn_multi_check_dark
endcase
goto, label_the_end

;;----------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------

label_sn140 : 

camera = 'gold'
cameras = ['gold','xeva','flir']

for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]

case camera of 
    'xeva' : exp = '0500'
    else   : exp = '0140'
endcase

root = '/mnt/HDD3TBn51/DST/vs/20220317/'
folder = root + camera + '/'
print, 'processing folder : ', folder


fdatas = findfile(folder+'/exp5_'+exp+'_'+camera+'*.fits')
fdarks = findfile(folder+'/dark_e'+exp+'*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'
nf =  n_elements(fdatas)


print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2]
if ss[0] eq 3 then dark = reform(rebin(dark, nx, ny, 1)) $
else dark = reform(rebin(dark, nx, ny))

print, 'calculating sn data ...'
img = float(readfits(fdatas[0],hd, /silent))
ss = size(img) & nx = ss[1] & ny = ss[2] & nt = ss[3]
for i=0,nt-1 do img[*,*,i] = img[*,*,i] - dark 

arr_mean = mean(img, dimension=3, /double)
arr_stdv = stddev(img, dimension=3, /double)

print, 'minmax of mean values = ', minmax(arr_mean)

if 1b then begin
res = hash()
res['arr_mean'] = arr_mean
res['arr_stdv'] = arr_stdv
fname = savedir+'/sn/'+camera+'.v'+version+'.sav'
save, res, filename=fname
print, 'saved as : ', fname
endif

endfor


stop

;;----------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------
visual1 :

;        ['white'  , 'green'  , 'red'    , 'yellow']
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x
xr = [0,18000]
cameras = ['gold','xeva','flir']

for kk=0, n_elements(cameras)-1 do begin
    camera = cameras[kk]
    fname = savedir+'/sn/'+camera+'.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname
    arr_mean = res['arr_mean']
    arr_stdv = res['arr_stdv']
    ss = size(arr_mean) & nx = ss[1] & ny = ss[2]
    arr_mean = reform(arr_mean, nx*ny)
    arr_stdv = reform(arr_stdv, nx*ny)

    window, kk+2, xs=800, ys=600
    plot, arr_mean, arr_stdv, psym=3, color=colors[0], xtitle='mean', ytitle='stddev', title=camera, charsize=charsize, xr=xr, yr=[0,200]
    fname = savedir  + '/sn/' + camera+'.sn.all.png'
    WRITE_PNG, fname, TVRD(/TRUE)
    ;wdelete, kk+2

    window, kk+9, xs=800, ys=600
    ;plot, arr_mean, arr_stdv/arr_mean, psym=3, color=colors[0], xtitle='mean', ytitle='stddev/mean', title=camera, charsize=charsize, xr=xr, yr=[0,0.1]
    plot, arr_mean, arr_stdv/arr_mean,/xlog,/ylog, psym=3, color=colors[0], xtitle='mean', ytitle='stddev/mean', title=camera, charsize=charsize, xr=[1E0, 2*1E4], yr=[1E-4,1E0]
    fname = savedir  + '/sn/' + camera+'.snnorm.all.png'
    WRITE_PNG, fname, TVRD(/TRUE)
    ;wdelete, kk+2
endfor

label_sn_multi_check_dark:

version = '0317'
cameras = ['gold','xeva','flir']

;;-- loop over camera
for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]

case camera of 
    'xeva' : exps = ['0050','0100','0200','0500']
    else   : exps = ['0015','0050','0100','0140']
endcase

root = '/mnt/HDD3TBn51/DST/vs/20220317/'
folder = root + camera + '/'
print, 'processing folder : ', folder

darks = fltarr(640,512,100,4)
;window, kk+18, xs=800, ys=600
window, kk+12, xs=640, ys=512
;;-- loop over exposure
for jj=0, n_elements(exps)-1 do begin
;exp = exps[jj]
exp = exps[n_elements(exps)-1-jj]

fdatas = findfile(folder+'/exp5_'+exp+'_'+camera+'*.fits')
fdarks = findfile(folder+'/dark_e'+exp+'*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'
nf =  n_elements(fdatas)


print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
darks[*,*,*,n_elements(exps)-1-jj] = dark
;print, hd
ss = size(dark) & nx=ss[1] & ny=ss[2]
if ss[0] eq 3 then dark = reform(rebin(dark, nx, ny, 1))

pdf = histogram(dark, LOCATIONS=xbin,BINSIZE=10)
;if jj eq 0 then begin
;plot, xbin, pdf, line=-1, color=colors[jj], xtitle='mean', ytitle='count', title=camera, charsize=charsize,yr=[0,5E4],xr=[-5E2,+2E3];,xr=[-1E3,+1.6E4]
;endif else begin
;oplot, xbin, pdf, line=-1, color=colors[jj]
;endelse

;; plot mean curve
if jj eq 0 then begin
plot, rebin(darks[*,*,*,n_elements(exps)-1-jj],1,1,100,1), color=colors[jj], xtitle='#frame', ytitle='mean', title=camera, charsize=charsize,yr=[1000,2000];,xr=[-5E2,+2E3];,xr=[-1E3,+1.6E4]
endif else begin
oplot, rebin(darks[*,*,*,n_elements(exps)-1-jj],1,1,100,1), color=colors[jj]
endelse

zero_count = ulonarr(100)
for jk=0, 100-1 do begin
pos = where(darks[*,*,jk,n_elements(exps)-1-jj] eq 0, count) 
zero_count[jk] = count
endfor


;for jk=0,100-1 do begin
;;if zero_count[jk] eq 0 then continue
;print, 'save exp=', exp, '[ms]  dark no.', jk
;tvscl, darks[*,*,jk,n_elements(exps)-1-jj]
;fname = savedir  + '/sn/png/' + camera+'.sn.check_dark.'+exp+'.'+strtrim(jk,2)+'.png'
;WRITE_PNG, fname, TVRD(/TRUE)
;endfor

;if jj eq 0 then begin
;plot, zero_count, line=1, color=colors[jj], xtitle='#frame', ytitle='zero count', title=camera, charsize=charsize;,yr=[0,5E4],xr=[-5E2,+2E3];,xr=[-1E3,+1.6E4]
;endif else begin
;oplot, zero_count, line=1, color=colors[jj]
;endelse

endfor ;;-- end loop over exposure
stop
endfor ;;-- end loop over camera


stop

;;----------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------

label_sn_multi:

_IS_SAVE = 0b

version = '0317'
version = '0320'
case version of 
    '0320' : cameras = ['gold','flir']
     else  : cameras = ['gold','xeva','flir']
endcase

;;-- loop over camera
for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]

;if camera ne 'flir' then continue

case version of 
    '0320' : begin
        root = '/mnt/HDD3TBn52/DST/vs/20220320/'
        folder = root + camera + '/'
        exps = ['0015','0050','0100','0200']
    end
    else : begin
        root = '/mnt/HDD3TBn51/DST/vs/20220317/'
        folder = root + camera + '/'
        case camera of 
            'xeva' : exps = ['0050','0100','0200','0500']
            else   : exps = ['0015','0050','0100','0140']
        endcase
    end
endcase
;if camera eq 'flir' then folder += '0318.v1/'

print, 'processing folder : ', folder


;;-- loop over exposure
for jj=0, n_elements(exps)-1 do begin
exp = exps[jj]

fdatas = findfile(folder+'/exp5_'+exp+'_'+camera+'*.fits')
fdarks = findfile(folder+'/dark_e'+exp+'*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'
nf =  n_elements(fdatas)


print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2]
if ss[0] eq 3 then dark = reform(rebin(dark, nx, ny, 1))

print, 'calculating sn data ...'
;img0 = readfits(fdatas[0],hd, /silent)
img  = float(readfits(fdatas[0],hd, /silent))
;if exp eq '0200' then stop
ss = size(img) & nx = ss[1] & ny = ss[2] & nt = ss[3]
for i=0,nt-1 do img[*,*,i] = img[*,*,i] - dark 

arr_mean = mean(img, dimension=3, /double)
arr_stdv = stddev(img, dimension=3, /double)

print, 'minmax of mean values = ', minmax(arr_mean)


;; calculate conversion factor =  (DN mean) / (DN stddev)   in unit of [e-/DN]
if exp eq '0100' then begin
    arr_mean_1d = reform(arr_mean, nx*ny)
    arr_stdv_1d = reform(arr_stdv, nx*ny)
    mask = where((arr_mean_1d gt 7000) and (arr_mean_1d lt 7500), count)
    print, camera , ' conversion factor calculated using mean of ', count, ' pixels'
    print, camera , ' --> ', mean(arr_mean_1d[mask]) / mean(arr_stdv_1d[mask])
endif

if _IS_SAVE then begin
res = hash()
res['arr_mean'] = arr_mean
res['arr_stdv'] = arr_stdv
fname = savedir+'/sn/'+camera+'.e'+exp+'.sn.v'+version+'.sav'
save, res, filename=fname
print, 'saved as : ', fname
endif

;UNDEFINE, dark, img, ss, arr_mean, arr_stdv, res, _IS_SAVE
endfor
;;-- end loop over exposure
endfor
;;-- end loop over camera

stop

;;----------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------

visual1_multi :

_IS_SAVE = 1b

version = '0317'
version = '0320'
case version of 
    '0320' : cameras = ['gold','flir']
     else  : cameras = ['gold','xeva','flir']
endcase

psym = 3

wid_bin = 1
;window, wid_bin, xs=800, ys=600
for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]
case camera of 
    'xeva' : exps = ['0050','0100','0200','0500']
    else   : exps = ['0015','0050','0100','0140']
endcase

case version of 
    '0320' : exps = ['0015','0050','0100','0200']
    else : begin
        case camera of 
            'xeva' : exps = ['0050','0100','0200','0500']
            else   : exps = ['0015','0050','0100','0140']
        endcase
    end
endcase

;goto, label_debug
;; plot stdv vs mean
window, kk+2, xs=800, ys=600

for jj=0, n_elements(exps)-1 do begin
    exp = exps[n_elements(exps)-1-jj]

    fname = savedir+'/sn/'+camera+'.e'+exp+'.sn.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname
    arr_mean = res['arr_mean']
    arr_stdv = res['arr_stdv']
    ss = size(arr_mean) & nx = ss[1] & ny = ss[2]
    arr_mean = reform(arr_mean, nx*ny)
    arr_stdv = reform(arr_stdv, nx*ny)

    if jj eq 0 then begin
    plot, arr_mean, arr_stdv, psym=psym, color=colors[jj], xtitle='mean', ytitle='stddev', title=camera, charsize=charsize, xr=[0,18000], yr=[0,200]
    endif else begin
    oplot, arr_mean, arr_stdv, psym=psym, color=colors[jj]
    endelse
endfor
fname = savedir  + '/sn/' + camera+'.sn.all.png'
if _IS_SAVE then WRITE_PNG, fname, TVRD(/TRUE)

;; plot stdv/mean vs mean
window, kk+9, xs=800, ys=600
for jj=0, n_elements(exps)-1 do begin
    exp = exps[n_elements(exps)-1-jj]

    fname = savedir+'/sn/'+camera+'.e'+exp+'.sn.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname
    arr_mean = res['arr_mean']
    arr_stdv = res['arr_stdv']
    ss = size(arr_mean) & nx = ss[1] & ny = ss[2]
    arr_mean = reform(arr_mean, nx*ny)
    arr_stdv = reform(arr_stdv, nx*ny)

    
    if jj eq 0 then begin
    plot, arr_mean, arr_stdv/arr_mean,/xlog,/ylog, psym=psym, color=colors[jj], xtitle='mean', ytitle='stddev/mean', title=camera, charsize=charsize, xr=[1E0, 2*1E4], yr=[1E-4,1E0]
    endif else begin
    oplot, arr_mean, arr_stdv/arr_mean, psym=psym, color=colors[jj]
    endelse
endfor
fname = savedir  + '/sn/' + camera+'.snnorm.all.png'
if _IS_SAVE then WRITE_PNG, fname, TVRD(/TRUE)

;label_debug:

bin=500
bin_start = 0
bin_end = 16000
bins1 = indgen( (bin_end-bin_start)/bin, increment=bin, start=bin_start )
bins2 = bins1 + bin
nbin = n_elements(bins1)
x = (bins1 + bins2) / 2

;wset, wid_bin
window, kk+14, xs=800, ys=600
for jj=0, n_elements(exps)-1 do begin
    exp = exps[n_elements(exps)-1-jj]

    fname = savedir+'/sn/'+camera+'.e'+exp+'.sn.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname
    arr_mean = res['arr_mean']
    arr_stdv = res['arr_stdv']
    ss = size(arr_mean) & nx = ss[1] & ny = ss[2]
    arr_mean = reform(arr_mean, nx*ny)
    arr_stdv = reform(arr_stdv, nx*ny)

    arr_ns = arr_stdv / arr_mean
    arr_bin_mean = fltarr(nbin)
    arr_bin_ns   = fltarr(nbin)
    ;; bining data
    mpos = []
    for kb=0, nbin-1 do begin
        mask = where( (arr_mean ge bins1[kb]) and (arr_mean le bins2[kb]), maskcount )
        if maskcount ge 1000 then mpos = [mpos,kb] else continue
        arr_bin_mean[kb] = mean(arr_mean[mask])
        arr_bin_ns[kb]   = mean(arr_ns[mask])
    endfor
    if jj eq 0 then begin
    plot, arr_bin_mean[mpos], arr_bin_ns[mpos],/xlog,/ylog, color=colors[jj], xtitle='mean', ytitle='stddev/mean', title='', charsize=charsize, xr=[1E0, 2*1E4], yr=[1E-4,1E0]
    oplot, arr_bin_mean[mpos], arr_bin_ns[mpos],psym=psyms[kk+2],color=colors[jj], symsize=1
    endif else begin
    oplot,arr_bin_mean[mpos], arr_bin_ns[mpos], color=colors[jj]
    oplot, arr_bin_mean[mpos], arr_bin_ns[mpos],psym=psyms[kk+2],color=colors[jj], symsize=1
    endelse
endfor

fname = savedir  + '/sn/' + camera+'.sn.flir_vs_gold.png'
if _IS_SAVE then WRITE_PNG, fname, TVRD(/TRUE)


endfor ;;-- end loop over cameras

stop

;;----------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------


label_hist_plot_multi:

version = '0317'
cameras = ['gold','xeva','flir']
psym = 3

for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]
case camera of 
    'xeva' : exps = ['0050','0100','0200','0500']
    else   : exps = ['0015','0050','0100','0140']
endcase

window, kk+7, xs=1600, ys=600
for jj=0, n_elements(exps)-1 do begin
    exp = exps[n_elements(exps)-1-jj]

    fname = savedir+'/sn/'+camera+'.e'+exp+'.sn.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname
    arr_mean = res['arr_mean']
    ;arr_stdv = res['arr_stdv']
    ss = size(arr_mean) & nx = ss[1] & ny = ss[2]
    arr_mean = reform(arr_mean, nx*ny)
    ;arr_stdv = reform(arr_stdv, nx*ny)

    pdf = histogram(arr_mean, LOCATIONS=xbin,BINSIZE=10)
    if jj eq 0 then begin
    plot, xbin, pdf, line=-1, color=colors[jj], xtitle='mean', ytitle='count', title=camera, charsize=charsize,yr=[0,5E4],xr=[-1E3,+1E4];,xr=[-1E3,+1.6E4]
    endif else begin
    oplot, xbin, pdf, line=-1, color=colors[jj]
    endelse
endfor
stop
endfor
stop


label_check_gold:

version = '0317'
psym = 3
_SHOWPLOT = 1b


camera = 'gold'

;- window, 2, xs=800, ys=600

exp = '0140'

fname = savedir+'/sn/'+camera+'.e'+exp+'.sn.v'+version+'.sav'
restore, fname
print, 'restored : ', fname
arr_mean = res['arr_mean']
arr_stdv = res['arr_stdv']
ss = size(arr_mean) & nx = ss[1] & ny = ss[2]
arr_mean = reform(arr_mean, nx*ny)
arr_stdv = reform(arr_stdv, nx*ny)


;stop

if _SHOWPLOT then plot, arr_mean, arr_stdv, psym=psym, color=colors[0], xtitle='mean', ytitle='stddev', title=camera, charsize=charsize, xr=[0,18000], yr=[0,200]

c0 = 15 & c1 = 0.0016
x = [0,15000]
if _SHOWPLOT then  oplot, x, c1*x+c0, color=colors[1]

mask = where( (arr_stdv le arr_mean*c1+c0) and (arr_mean le 1.45E4) )
if _SHOWPLOT then  oplot, arr_mean[mask], arr_stdv[mask], psym=psym, color=colors[1]

;fname = savedir  + '/sn/' + camera+'.sn.check.scatter.png'
;WRITE_PNG, fname, TVRD(/TRUE)

arr_mask = arr_mean
arr_mask[*] = 0
for i=0,n_elements(mask)-1 do arr_mask[ mask[i] ] = 1
arr_mask = reform(arr_mask, nx, ny)
if _SHOWPLOT then window, 3, xs=nx, ys=ny
if _SHOWPLOT then tvscl, arr_mask

;fname = savedir  + '/sn/' + camera+'.sn.check.tvscl.png'
;WRITE_PNG, fname, TVRD(/TRUE)


if 0b then begin
window, 7, xs=nx, ys=ny
arr_mean_2d = reform(arr_mean, nx, ny)
mean_tmp = arr_mean_2d - median(arr_mean_2d,2)
mean_tmp_m = mean(mean_tmp)

; flick procedure : blink image A and image B with a given framerate
flick, bytscl(mean_tmp<(mean_tmp_m+1000)>(mean_tmp_m-100)), bytscl(arr_mask), 1

endif


stop

label_the_end : 
END
