
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/20220220.orca-rolling-shutter-experiment/'
goto, visual

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
data = float(readfits(fdatas[0],hd))
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
np = ny-ny/2-1
ys = indgen(np,start=ny/2,increment=1)
amps = fltarr(np)
phs = fltarr(np)
profls = fltarr(np,nt)
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
iymax = 500
window, 2, xs=800, ys=600
plot, ys[0:iymax],phs[9:iymax], xtitle='y-pixel', ytitle='phase', yr=minmax(phs[9:iymax])

res = hash()
res['iymax'] = iymax
res['ys'] = ys
res['phs'] = phs
res['amps'] = amps
res['th'] = th
res['profls'] = profls

fname = savedir+'/bin'+bin+'.exp'+exp+'ms.sav'
save, res, filename=fname
print, 'saved as : ', fname



visual:

exparr = ['50','15','5']
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
window, 3, xs=1200, ys=800
coefs = fltarr(2,3)
for i=0,n_elements(exparr)-1 do begin
    exp = exparr[i]
    print, '--------------------------------'
    print, 'bin=', bin, ' exp=', exp, '[ms]'
    fname = savedir+'/bin'+bin+'.exp'+exp+'ms.sav'
    print, 'restored', fname
    restore, fname
    ;th = res['th']
    iymax = res['iymax']
    phs = res['phs']
    if phs[0] lt yr[0] then phs[*] += !pi
    ys = res['ys']
    print, 'minmax : ', minmax(phs)
    xx = ys[0:iymax] & yy = phs[0:iymax]
    coef = poly_fit(xx,yy,1)
    coefs[*,i] = coef
    print, 'f = y * ', coef[1],' + ', coef[0]
    if i eq 0 then begin 
        plot, xx, yy, yr=yr, color=colors[i], psym=2, symsize=0.1
    endif
    oplot, xx, yy, color=colors[i], psym=2, symsize=0.1
    oplot, xx, coef[0]+coef[1]*xx, color=colors[i]
endfor





END