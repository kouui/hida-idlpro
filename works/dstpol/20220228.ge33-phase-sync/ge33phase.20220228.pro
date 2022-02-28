
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/20220228.ge33-phase-sync/'
charsize=1.5



exp = '50' ;; [ms]
rotps = '1' ;; [sec]
rotp = float(rotps) ;; [sec]

goto, visual_phase

x1 = 150
x2 = 220


xr = [0, 50]
;case exp of 
;    '50': xr = []
;endcase


date = '20220228'
spec = 'K_I_6697'
root = '/mnt/HDD3TBn51/DST/sp/'+date+'/'
;folder = root+'/exp'+exp+'ms/'
folder = root+'/after-revise'+'/exp'+exp+'ms/'
print, 'processing folder : ', folder

;goto, debug



fdatas = findfile(folder+'/*.fits')
fdarks = findfile(root+'/darks/dark_'+exp+'*.fits')

print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'


print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2] & nt=ss[3]
dark = reform(rebin(dark, nx, ny, 1))

if 0b then begin
print, 'reading data ...'
data = float(readfits(fdatas[0],hd))
print, 'data - darks ...'
for i=0, nt-1 do data[*,*,i] = data[*,*,i] - dark
endif

nf = n_elements(fdatas)
curve = fltarr(nt,nf)
for k=0, nf-1 do begin
    data = float(readfits(fdatas[k],hd))
    for i=0, nt-1 do data[*,*,i] = data[*,*,i] - dark
    curve[*,k] = reform(rebin(data[x1:x2,*,*],1,1,nt))
endfor

debug:


window, 1, xs=800, ys=600
for i=0, nf-1 do begin
    if i eq 0 then plot, curve[*,i], charsize=charsize, xr=xr, xtitle='rotation step', ytitle='mean counting' $
    else  oplot, curve[*,i]
endfor

if 1b then begin
res = hash()
res['curve'] = curve
fname = savedir+'exp'+exp+'ms.sav'
save, res, filename=fname
print, 'saved as : ', fname
endif

stop

visual_phase:

exps = ['10','15','30','50']
nexp = n_elements(exps)
nf = 40
ph1s = fltarr(nf,nexp)
k1s  = fltarr(nf,nexp)
dts  = fltarr(nf,nexp)
for i=0, nexp-1 do begin
    exp = exps[i]
    fname = savedir+'exp'+exp+'ms.sav'
    restore, fname
    print, '--------------------------------------------------------'
    print, 'restored from : ', fname
    curve = res['curve']
    UNDEFINE, res
    ss = size(curve) & nt = ss[1]; & nf=ss[2]
    print, 'nt=', nt, ' nf=',nf
    th = findgen(nt) * fix(exp) * 1E-3 / rotp * 2*!pi
    for k=0, nf-1 do begin
        fit1 = ta_sinfit_mpfit(th,curve[*,k],av=av1,amp=amp1,k=k1,ph=ph1)
        print, 'exp=', exp, ' k1=',k1, ' ph1=', ph1
        
        ph1s[k,i] = ph1
        k1s[k,i] = k1
        dts[k,i] = ph1/4.0 * ( rotp / (2*!pi) ) * 1E3
    endfor

endfor
;        ['white'  , 'green'  , 'red'    , 'yellow']
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x

window, 2, xs=800, ys=600
ytitle = 'phase [rad]'
data = ph1s
;yr = [0.5,2.5]
yr = [-1,0]
for i=0, nexp-1 do begin
    if i eq 0 then plot, data[*,i], charsize=charsize, color=colors[i],yr=yr, xtitle='# dataset', ytitle=ytitle $
    else  oplot, data[*,i], color=colors[i]
endfor

window, 3, xs=800, ys=600
ytitle = 'time [msec]'
data = dts
;yr = [30,90]
yr = [-40,10]
for i=0, nexp-1 do begin
    if i eq 0 then plot, data[*,i], charsize=charsize, color=colors[i],yr=yr, xtitle='# dataset', ytitle=ytitle $
    else  oplot, data[*,i], color=colors[i]
endfor

window, 4, xs=800, ys=600
ytitle = 'frequency [hz]'
data = k1s
yr = [3.5,4.5]
for i=0, nexp-1 do begin
    if i eq 0 then plot, data[*,i], charsize=charsize, color=colors[i],yr=yr, xtitle='# dataset', ytitle=ytitle $
    else  oplot, data[*,i], color=colors[i]
endfor



END