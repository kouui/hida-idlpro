
@lib.dstpol.dispiquvr

function byte2hex, val
    vh = dec2hex(val)
    if val < 16 then vh = '0'+vh
    return, vh[0:1]
end

rootdir = '/nwork/kouui/data-lvl1/dstpol/20220518.prom-i2quv/'
datadir = rootdir + 'pr.1.1/sav.s0/'

s0files = find_file(datadir + '*.s0.sav')
ns0f = n_elements(s0files)
print, "found ", ns0f ," *.s0.sav"
if (ns0f eq 1) then if (s0files eq '') then throw_error, 's0 file not found !!!'



file = s0files[200]
restore, file, /verbose
ss = size(s0)
nx = ss[1]
ny = ss[2]
undefine, ss

gotolabel = 1

case gotolabel of 
    0: goto, label_sample
    1: goto, label_varSlit_alignConti
endcase

label_sample:

pmax = 0.01
bin=1
yrc = [130,150]
ku_dispiquvr,s0,bin=bin,pmax=pmax*max(s0[*,*,0]),wid=11
k = 100
window, 10, xs=800,ys=600
plot, s0[k,*,0] / mean(s0[k,yrc[0]:yrc[1],0])
k = 200
window, 9, xs=800,ys=600
plot, s0[k,*,0] / mean(s0[k,yrc[0]:yrc[1],0])

stop

label_varSlit_alignConti:

stoke=1

_plot_step = 20

xr = [20,nx-1-20]
yr = [130,200-1]
yr = [0,450]
nyr = yr[1]-yr[0]+1
window, 22, xs=800,ys=800
for i=xr[0], xr[1] do begin
    if (i mod _plot_step) ne 0 then continue
    pran = indgen(nyr)+yr[0]
    prof = s0[i,yr[0]:yr[1],stoke]
    if i eq xr[0] then begin
        plot , pran, prof, xstyle=1, yr=[-2,1.1*max(prof)], color='ffffff'x
	plot , pran, prof, xstyle=1, yr=[-10,10], color='ffffff'x
    endif else begin
        color = 256l*256l*256l-1-256l*1l*fix((i-xr[0])*(255.0-0.0)/(xr[1]-xr[0]))
        oplot, pran, prof, color= color
    endelse
endfor

yrc = [130,150]
window, 23, xs=800,ys=800
for i=xr[0], xr[1] do begin
    if (i mod _plot_step) ne 0 then continue
    pran = indgen(nyr)+yr[0]
    prof = s0[i,yr[0]:yr[1],stoke]
    prof = prof / mean(prof[yrc[0]-yr[0]:yrc[1]-yr[0]])
    if i eq xr[0] then begin
        prof0 = prof
        plot , pran, prof, xstyle=1, yr=[0,1.1*max(prof)], color='ffffff'x
	  ;plot , pran, prof, xstyle=1, yr=[0,1.1*max(prof)], color='ffffff'x
    endif else begin
        color = 256l*256l*256l-1-256l*1l*fix((i-xr[0])*(255.0-0.0)/(xr[1]-xr[0]))
        oplot, pran, prof, color=color
    endelse
endfor

END
