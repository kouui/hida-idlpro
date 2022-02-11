;@mmdst
;@dst_pollib

function clip, arr, vmax, vmin
    ma = max(arr)
    mi = min(arr)
    return, (arr-mi) * (vmax-vmin)/(ma-mi) + vmin
end

path0 = "/tmp_mnt/nwork/kouui/data-lvl1/dstpol/20220110.giant-prominence/spec/ar1"
path = path0 + "/iquv"
files = findfile(path+'/*.sav')
nf = n_elements(files)
print, "found ", nf , " save files"

stop

file = files[0]
restore, file
ss = size(s3) & nx = ss[1] & ny = ss[2] & ns = ss[3]
s4 = fltarr(nx,ny,ns,nf)
s4[*,*,*,0] = s3[*,*,*]

for i=1,nf-1 do begin
    file = files[i]
    restore, file
    s4[*,*,*,i] = s3[*,*,*]
endfor

ypos1 = 451 ; Si, Vmax, red
ypos2 = 339 ; He, Vmax, red

y = ypos2
pmax = 0.05
window, 1, xsize=nx,ysize=nf

;tvscl,clip(s4[*,y,3,*],pmax,-pmax)
tvscl,s4[*,y,0,*]
filename= path0 + "/he.I.png"
WRITE_PNG,filename,TVRD(/TRUE)







END