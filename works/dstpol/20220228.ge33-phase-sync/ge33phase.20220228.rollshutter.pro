
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/20220228.ge33-phase-sync/'
charsize=1.5

exp = '50' ;; [ms]
rotps = '1' ;; [sec]
rotp = float(rotps) ;; [sec]

goto, visual_phase

x1 = 150
x2 = 220

xr = [0, 50]

date = '20220228'
spec = 'K_I_6697'
root = '/mnt/HDD3TBn51/DST/sp/'+date+'/'
;folder = root+'/exp'+exp+'ms/'
folder = root+'/after-revise'+'/exp'+exp+'ms/'
print, 'processing folder : ', folder


fdatas = findfile(folder+'/*.fits')
fdarks = findfile(root+'/darks/dark_'+exp+'*.fits')

print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'


print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2] & nt=ss[3]
dark = reform(rebin(dark, nx, ny, 1))

nf = n_elements(fdatas)
curve = fltarr(ny,nt)

data = float(readfits(fdatas[0],hd))
for i=0, nt-1 do data[*,*,i] = data[*,*,i] - dark
curve[*,*] = reform(rebin(data[x1:x2,*,*],1,ny,nt))

th = findgen(nt) * fix(exp) * 1E-3 / rotp * 2*!pi
phs  = fltarr(ny)
amps = fltarr(ny)
for i=0, ny-1 do begin
    fit1 = ta_sinfit_mpfit(th,curve[i,*],av=av1,amp=amp1,k=k1,ph=ph1)
    phs[i] = ph1
    amps[i]= amp1
endfor

if 0b then begin
yr = [mean(phs)*0.8,mean(phs)*1.2]
window, 1, xs=800, ys=600
plot, phs, charsize=charsize, yr=yr, xtitle='y-pixel', ytitle='phase [rad]'
endif

if 1b then begin
res = hash()
res['phs'] = phs
res['amps'] = amps
res['curve'] = curve
fname = savedir+'exp'+exp+'ms.roll.sav'
save, res, filename=fname
print, 'saved as : ', fname
endif

visual_phase:

exps = ['10','15','30','50']
nexp = n_elements(exps)
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x

window, 4, xs=800, ys=600
yr = [-1, 0]
yr = [-0.6,-0.5]
for i=0, nexp-1 do begin
    exp = exps[i]
    fname = savedir+'exp'+exp+'ms.roll.sav'
    restore, fname
    print, '--------------------------------------------------------'
    print, 'restored from : ', fname
    phs = res['phs']
    if i eq 0 then plot, phs, charsize=charsize, color=colors[i], yr=yr, xtitle='y-pixel', ytitle='phase [rad]' $
    else oplot, phs, color=colors[i]
endfor

; result : standard deviation of each phs is ~ 0.02 [rad]
; corresponds to  0.02/4 * 1/(2*!pi) ~ 0.0008 [sec] = 0.8[ms]
; maybe not rolling shutter
END