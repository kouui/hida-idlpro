function dateob2float, str
    str1 = strmid(str,21,12)
    n  = double(strmid(str1,0,2))*3600d
    n += double(strmid(str1,3,2))*60d
    n += double(strmid(str1,6,2))*1d
    n += double(strmid(str1,9,3))*0.001d
    return, n
end

date = '20220315'
identifier = 'orca-phase-sync'
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'+date+'.'+identifier+'/'
charsize=1.5
version = '0315'

cameras = ['camera1','camera2']

goto, visual2

;camera = 'camera1'
for kk=0,1 do begin
camera = cameras[kk]

root = '/mnt/HDD3TBn51/DST/sp/'+date+'/'
folder = root + camera + '/pol2/'
print, 'processing folder : ', folder

case camera of 
    'camera1': begin 
        x1 = 1350
        x2 = 1600
    end
    'camera2': begin 
        x1 = 1350
        x2 = 1550
    end
endcase
y0 = 512

fdatas = findfile(folder+'/orca*.fits')
case camera of 
    'camera1' : fdarks = findfile(folder+'/dark*.fits')
    'camera2' : fdarks = findfile(folder+'/dark*.fits')
endcase

print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'

print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2] & nt=ss[3]
dark = reform(rebin(dark, nx, ny, 1))


print, 'reading data ...'
nf = n_elements(fdatas)
;nf=1
curve = fltarr(nt,nf)
dateob2 = dblarr(nf)
dateob2str = []
for k=0, nf-1 do begin
    data = float(readfits(fdatas[k],hd))
    for i=0, nt-1 do data[*,*,i] = data[*,*,i] - dark
    curve[*,k] = reform(rebin(data[x1:x2,y0,*],1,1,nt))
    dateob2[k] = dateob2float(hd[8])
    dateob2str = [hd[8],dateob2str]
endfor

if 1b then begin
res = hash()
res['curve'] = curve
res['dateob2'] = dateob2
res['dateob2str'] = dateob2str
fname = savedir+camera+'.v'+version+'.sav'
save, res, filename=fname
print, 'saved as : ', fname
endif

endfor

stop

visual1 :

;        ['white'  , 'green'  , 'red'    , 'yellow']
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x
xr = [-2,20]
for kk=0,1 do begin
    window, kk+3, xs=800, ys=600
    if kk eq 0 then yr=[0,5000] else yr=[0, 4E4]
    camera = cameras[kk]
    fname = savedir+camera+'.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname
    curve = res['curve']
    ;dateob2 = res['dateob2']
    
    ss = size(curve) & nt = ss[1] & nf = ss[2]
    count = 0
    for i=0,nf-1 do begin
        if i eq 0  then begin
            plot, curve[*,i], charsize=charsize, color=colors[0],xr=xr,yr=yr, xtitle='# dataset', ytitle='', line=2
            oplot, curve[*,i], color=colors[0], psym=2
        endif else begin
            oplot, curve[*,i], color=colors[0], line=2
            oplot, curve[*,i], color=colors[0], psym=2
        endelse
        if kk eq 0 then $
        if curve[1,i] gt 2E3 then begin
            count += 1
            print, i
        endif
    endfor
    if kk eq 0 then print, 'normal count = ', count, '/', nf
endfor

stop

visual2 :

;        ['white'  , 'green'  , 'red'    , 'yellow']
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x
window, 8, xs=800, ys=600
yr=[50000,51000]
xr=[0,60]
for kk=0,1 do begin

    camera = cameras[kk]
    fname = savedir+camera+'.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname
    ;curve = res['curve']
    dateob2str = res['dateob2str']
    dateob2 = dblarr(n_elements(dateob2str))
    for i=0,n_elements(dateob2str)-1 do dateob2[i] = dateob2float(dateob2str[n_elements(dateob2str)-1-i])

    ss = size(dateob2) & nf = ss[1]

    if kk eq 0 then begin
        plot, dateob2, charsize=charsize, color=colors[kk],xr=xr,yr=yr, xtitle='# dataset', ytitle='dateob2 total seconds', line=2
        oplot, dateob2, color=colors[kk], psym=2
    endif else begin
        oplot, dateob2, color=colors[kk], line=2
        oplot, dateob2, color=colors[kk], psym=2
    endelse

endfor

window, 10, xs=800, ys=600
;yr=[50000,51000]
xr=[0,60]
for kk=0,1 do begin

    camera = cameras[kk]
    fname = savedir+camera+'.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname
    ;curve = res['curve']
    dateob2str = res['dateob2str']
    dateob2 = dblarr(n_elements(dateob2str))
    for i=0,n_elements(dateob2str)-1 do dateob2[i] = dateob2float(dateob2str[n_elements(dateob2str)-1-i])

    ss = size(dateob2) & nf = ss[1]
    dateob2 = dateob2[1:-1] - dateob2[0:-2]

    if kk eq 0 then begin
        plot, dateob2, charsize=charsize, color=colors[kk],xr=xr, xtitle='# dataset', ytitle='dateob2 total seconds', line=2
        oplot, dateob2, color=colors[kk], psym=2
    endif else begin
        oplot, dateob2, color=colors[kk], line=2
        oplot, dateob2, color=colors[kk], psym=2
    endelse

endfor

window, 9, xs=800, ys=600
xr=[0,60]
yr = [-1.5,1.5]
for kk=0,1 do begin
    camera = cameras[kk]
    fname = savedir+camera+'.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname

    dateob2str = res['dateob2str'] 
    dateob2 = dblarr(n_elements(dateob2str))
    if kk eq 0 then dateob22 = dblarr(n_elements(dateob2str),2)
    for i=0,n_elements(dateob2str)-1 do dateob2[i] = dateob2float(dateob2str[n_elements(dateob2str)-1-i])
    dateob22[*,kk] = dateob2
endfor
plot, dateob22[*,1]-dateob22[*,0], charsize=charsize, yr=yr, color=colors[0],xr=xr,xtitle='# dataset', ytitle='dateob2 time difference [sec]', psym=2


stop

visual3 :

for kk=0,1 do begin
    camera = cameras[kk]
    fname = savedir+camera+'.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname

    dateob2str = res['dateob2str'] 
    dateob2 = dblarr(n_elements(dateob2str))
    if kk eq 0 then dateob22 = dblarr(n_elements(dateob2str),2)
    for i=0,n_elements(dateob2str)-1 do dateob2[i] = dateob2float(dateob2str[n_elements(dateob2str)-1-i])
    dateob22[*,kk] = dateob2
endfor

dateob2diff = dateob22[*,1]-dateob22[*,0]

xpos = hash()
xpos['child-first'] = where(dateob2diff gt -1)
xpos['parent-first'] = where(dateob2diff lt -2)


;        ['white'  , 'green'  , 'red'    , 'yellow']
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x
xr = [-2,20]
for kk=0,0 do begin
    if kk eq 0 then yr=[1E3,2E4] else yr=[1E4, 6E4]
    camera = cameras[kk]
    fname = savedir+camera+'.v'+version+'.sav'
    restore, fname
    print, 'restored : ', fname
    curve = res['curve']
    ;dateob2 = res['dateob2']
    
    ss = size(curve) & nt = ss[1] & nf = ss[2]
    title = 'child-first'
    pos = xpos[title]
    window, 13, xs=800, ys=600
    for i=0,n_elements(pos)-1 do begin
        ix = pos[i]
        if i eq 0  then begin
            plot, curve[*,ix], charsize=charsize, color=colors[0],xr=xr,yr=yr,title=title, xtitle='# dataset', ytitle='', line=2
            oplot, curve[*,ix], color=colors[0], psym=2
        endif else begin
            oplot, curve[*,ix], color=colors[0], line=2
            oplot, curve[*,ix], color=colors[0], psym=2
        endelse
    endfor
    title = 'parent-first'
    pos = xpos[title]
    window, 14, xs=800, ys=600
    for i=0,n_elements(pos)-1 do begin
        ix = pos[i]
        if i eq 0  then begin
            plot, curve[*,ix], charsize=charsize, color=colors[0],xr=xr,yr=yr,title=title, xtitle='# dataset', ytitle='', line=2
            ;oplot, curve[*,ix], color=colors[1], line=2
            oplot, curve[*,ix], color=colors[0], psym=2
        endif else begin
            oplot, curve[*,ix], color=colors[0], line=2
            oplot, curve[*,ix], color=colors[0], psym=2
        endelse
    endfor
endfor

END
