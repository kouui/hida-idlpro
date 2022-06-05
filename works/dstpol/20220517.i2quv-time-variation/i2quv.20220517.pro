
@lib.dstpol.dispiquvr.pro
@my_colors

datadir = '/tmp_mnt/nwork/shirato/20220504/withAO/spot1/sav.s0'
files = findfile(datadir+'/*.s0.sav')
nf = n_elements(files)
print, "found ", nf, ' .s0.sav files'

restore, files[0]
ss = size(s0) & nx=ss[1] & ny=ss[2]


yl=149
x0=240   ; 30 100 150 190 240
dx0=3
yc=430
dy=30

nf = 900

dstarr = []
iquv50 = fltarr(ny,4,nf)
for i=0,nf-1 do begin
    restore, files[i]
    ;if i eq 50 then dispiquvr, s0, bin=1, pmax=
    for j=0,3 do begin
        iquv50[*,j,i] = rebin(s0[x0-dx0:x0+dx0,*,j],1,ny,1)
    endfor
    dstarr = [dstarr, [dst]]
endfor

if 0b then begin
window, 5, xs=800, ys=600
yr = [0.5,1.2]
for i=0,nf-1 do begin
    if i eq 0 then plot, iquv50[*,0,i],xstyle=1,yr=yr $
    else oplot, iquv50[*,0,i]
endfor
endif


conti=fltarr(nf,4)
atm =fltarr(nf,4)
si  =fltarr(nf,4)
for i=0,nf-1 do begin
    ;; find minpos
    ipf = reform(iquv50[*,0,i])
;    _ = min(ipf[140:160],pos) & pos+=140   ; H2O
    _ = min(ipf[77:97],pos) & pos+=77     ; H2O
;    _ = min(ipf[300:340],pos) & pos+=300  ; Si I
    for j=0,3 do begin
        atm[i,j] = rebin(iquv50[pos-1:pos+1,j,i],1,1,1)
;        si[i,j] = rebin(iquv50[pos1-1:pos1+1,j,i],1,1,1)
        conti[i,j] = rebin(iquv50[yc-dy:yc+dy,j,i],1,1,1)
    endfor 
endfor

window, 4, xs=800, ys=600
;plot, atm[*,0]/conti[*,0], color=_colors[0], yr=[0.1,0.5]
plot, atm[*,0]/conti[*,0], color=_colors[0], yr=[0.4,0.8]
;oplot, si[*,0]/conti[*,0], color=_colors[0], line=2
for j=1,3 do begin
oplot, atm[*,j]/conti[*,j], color=_colors[j]
;oplot, si[*,j]/conti[*,j], color=_colors[j], line=2
endfor

;; save output to a .sav file
if 0b then begin
outdir = '/nwork/kouui/data-lvl1/dstpol/20220517.i2quv-timeseries'
savf = outdir + '/atmconti.20220517.sav'

save, atm, conti, dstarr, files, filename=savf, /verbose
print, 'saved as :', savf

endif
; width = 5
; atm_avg = 0
; conti_avg = 0
; for i=0,width-1 do begin 
;     atm_avg += atm[i:i+nf-width-1,3]
;     conti_avg += conti[i:i+nf-width-1,3]
; end
; atm_avg /= width
; conti_avg /= width
; oplot, atm_avg/conti_avg, color=_colors[4]


END
