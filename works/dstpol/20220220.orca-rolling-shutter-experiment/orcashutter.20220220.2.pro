
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/20220220.orca-rolling-shutter-experiment/'
goto, visual1

bin = '2' 
exp = '5' ;; [ms]
rotp = 1. ;; [sec]

case bin of
    '1' : begin 
        x1=410
        x2=540
        bias=1030
        dx = x2-x1+1
        iymax = 600
    end
    '2' : begin
        x1=210
        x2=270
        bias=510
        dx = x2-x1+1
        iymax = 500
    end
endcase

root = '/mnt/HDD3TBn51/DST/sp/20220220/'
folder = root+'bin'+bin+'/'+exp+'ms/'
print, 'processing folder : ', folder
;savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/20220220.orca-rolling-shutter-experiment/'
;goto, debug

fdatas = findfile(folder+'orca*.fits')
fdarks= findfile(folder+'dark*.fits')

print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'


print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2] & nt=ss[3]
dark = reform(rebin(dark, nx, ny, 1))

print, 'reading data ...'
data = float(readfits(fdatas[1],hd))
print, 'data - darks ...'
for i=0, nt-1 do data[*,*,i] = data[*,*,i] - dark



print, 'ploting sample profile ...'
ys = indgen(50,start=50,increment=1) * ny /100
window, 1, xs=800, ys=600
for i=0,n_elements(ys)-1 do begin
    iy = ys[i]
    profl = reform(rebin(data[x1:x2,iy,*],1,1,nt))
    ;profr = reform(rebin(data[x1+bias:x2+bias,iy,*],1,1,nt))
    if i gt 0 then begin 
        oplot, profl
        continue
    endif
    plot, profl, xr=[0,30], xtitle='rotation step', ytitle='counting'
endfor


;debug:
print, 'calculating phase ...'
th = findgen(nt) * fix(exp) * 1E-3 / rotp * 2*!pi
;np = ny-ny/2-1
;ys = indgen(np,start=ny/2,increment=1)
np = ny-1
ys = indgen(np,start=0,increment=1)
amps = fltarr(np)
phs = fltarr(np)
profls = fltarr(np,nt)


; init values
profl = reform(rebin(data[x1:x2,*,*],1,1,nt))
fit1 = ta_sinfit_mpfit(th,profl,av=av1,amp=amp1,k=k1,ph=ph1)
;; fitting each y position
for i=0,np-1 do begin
    iy = ys[i]
    profl = reform(rebin(data[x1:x2,iy,*],1,1,nt))
    profls[i,*] = profl
    fit1 = ta_sinfit_mpfit(th,profl,av=av1,amp=amp1,k=k1,ph=ph1)
    ;if amp1 lt 0 then amp1 *= -1 & ph1 += !pi
    amps[i] = amp1
    phs[i]  = ph1
endfor

;debug:
;phs0 = phs
;for i=1, np-1 do begin
;    while phs[i] lt phs[0]-0.10*!pi do phs[i] += !pi
;endfor
;plot, ys,phs0, xtitle='y-pixel', ytitle='phase'

debug:
iymax = ny-2
window, 2, xs=800, ys=600
plot, ys[0:iymax],phs[0:iymax], xtitle='y-pixel', ytitle='phase', yr=minmax(phs[9:iymax])


res = hash()
res['iymax'] = iymax
res['ys'] = ys
res['phs'] = phs
res['amps'] = amps
res['th'] = th
res['profls'] = profls


fname = savedir+'/bin'+bin+'.exp'+exp+'ms.1.sav'
save, res, filename=fname
print, 'saved as : ', fname


stop
visual1:

exparr = ['5','15','50']
binarr = ['1','2']

bin = '1';binarr[2]
case bin of 
    '1' : begin
        yr = [2.8,3.4];[0,3.2];
    end
    '2' : begin
        yr = [0,2.2];[1.8,2.2]
    end
endcase
;           white      green      red
colors = ['ffffff'x, '66ff00'x, '9900ff'x];, '00ffff'x] ; 'bbggrr'x
window, 13, xs=2000, ys=1200
;;coefs = fltarr(2,3)
yr = [0,4]
count = 0
!p.multi=[0,3,2]
for j=0,n_elements(binarr)-1 do begin
bin = binarr[j]
case bin of 
    '1' : begin
        dy = 1024
        y0 = 1024
        fac = 1.0
        bias = 0.0
    end
    '2' : begin
        dy = 512
        y0 = 512
        fac = 0.25
        bias = 1000
    end
endcase
for i=0,n_elements(exparr)-1 do begin
    
    exp = exparr[i]
    ;if exp ne '15' then continue
    print, '--------------------------------'
    print, 'bin=', bin, ' exp=', exp, '[ms]'
    fname = savedir+'/bin'+bin+'.exp'+exp+'ms.1.sav'
    print, 'restored', fname
    restore, fname
    th = res['th']
    profls = res['profls']
    iymax = res['iymax']
    phs = res['phs']
    ;if phs[0] lt yr[0] then phs[*] += !pi
    for k=0,n_elements(phs)-1 do if phs[k] lt 0 then phs[k] += !pi*2
    ys = res['ys']
    ys[*] -= dy
    ;print, 'minmax : ', minmax(phs)
    xx = ys[0:iymax] & yy = phs[0:iymax]
    prf = profls[y0,*]
    UNDEFINE, av1, amp1, k1, ph1
    fit1 = ta_sinfit_mpfit(th,prf,av=av1,amp=amp1,k=k1,ph=ph1)
    if amp1 lt 0 then begin 
        amp1  *= -1
        ph1 += !pi
    endif
    print, 'amp1=', amp1, '  ph1=', ph1
    ;;coef = poly_fit(xx,yy,1)
    ;;coefs[*,i] = coef
    ;;print, 'f = y * ', coef[1],' + ', coef[0]
    if j eq 0 then psym=0 else psym=2
    ;if bin eq '1' then line=0 else line=2
    ;line = -1
    if (i eq 0) and (j eq 0) then begin 
        ;plot, xx, yy, yr=yr, color=colors[i], psym=psym, symsize=0.1
    endif
    if count eq 0 then begin 
        plot, prf, psym=2, xr=[0,50];, psym=psym, symsize=0.1
        oplot, fit1, line=0
    endif else begin
        plot, prf, psym=2, xr=[0,50]
        oplot, fit1, line=0
    endelse
    ;oplot, xx, yy, color=colors[i], psym=psym, symsize=0.1
    ;;oplot, xx, coef[0]+coef[1]*xx, color=colors[i]
    count += 1
endfor
endfor

stop
visual2:

exparr = ['5','15','50']
binarr = ['1','2']

bin = '1';binarr[2]
case bin of 
    '1' : begin
        yr = [2.8,3.4];[0,3.2];
    end
    '2' : begin
        yr = [0,2.2];[1.8,2.2]
    end
endcase
;           white      green      red
colors = ['ffffff'x, '66ff00'x, '9900ff'x];, '00ffff'x] ; 'bbggrr'x
window, 14, xs=2000, ys=1200
;;coefs = fltarr(2,3)
yr = [1.5,3.8]
count = 0
!p.multi=[0,1,1]
for j=0,n_elements(binarr)-1 do begin
bin = binarr[j]
case bin of 
    '1' : begin
        dy = 1024
        y0 = 1024
        fac = 1.0
        bias = 0.0
    end
    '2' : begin
        dy = 512
        y0 = 512
        fac = 0.25
        bias = 1000
    end
endcase
for i=0,n_elements(exparr)-1 do begin
    
    exp = exparr[i]
    ;if exp ne '15' then continue
    print, '--------------------------------'
    print, 'bin=', bin, ' exp=', exp, '[ms]'
    fname = savedir+'/bin'+bin+'.exp'+exp+'ms.1.sav'
    print, 'restored', fname
    restore, fname
    th = res['th']
    profls = res['profls']
    iymax = res['iymax']
    phs = res['phs']
    amps = res['amps']
    ;if phs[0] lt yr[0] then phs[*] += !pi
    for k=0,n_elements(phs)-1 do if phs[k] lt 0 then phs[k] += !pi*2
    for k=0,n_elements(phs)-1 do if amps[k] lt 0 then begin 
        amps[k] *= -1
        phs[k] += !pi
    endif
    ys = res['ys']
    ys[*] -= dy
    ;print, 'minmax : ', minmax(phs)
    xx = ys[0:iymax] & yy = phs[0:iymax]
    
    print, 'amp1=', amps[y0], '  ph1=', phs[y0]
    ;;coef = poly_fit(xx,yy,1)
    ;;coefs[*,i] = coef
    ;;print, 'f = y * ', coef[1],' + ', coef[0]
    if j eq 0 then psym=0 else psym=2
    ;if bin eq '1' then line=0 else line=2
    ;line = -1
    if (i eq 0) and (j eq 0) then begin 
        ;plot, xx, yy, yr=yr, color=colors[i], psym=psym, symsize=0.1
    endif
    if count eq 0 then begin 
        plot, xx, yy, yr=yr, color=colors[i]
    endif else begin
        oplot, xx, yy, color=colors[i]
    endelse
    ;oplot, xx, yy, color=colors[i], psym=psym, symsize=0.1
    ;;oplot, xx, coef[0]+coef[1]*xx, color=colors[i]
    count += 1
endfor
endfor




END