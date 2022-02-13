;@mmdst
;@dst_pollib

function clip, arr, vmax, vmin
    ma = max(arr)
    mi = min(arr)
    return, (arr-mi) * (vmax-vmin)/(ma-mi) + vmin
end

path0 = "/tmp_mnt/nwork/kouui/data-lvl1/dstpol/20220110.giant-prominence/spec/pr1"
path = path0 + "/iquv"
files = findfile(path+'/*.sav')
nf = n_elements(files)
print, "found ", nf , " save files"


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

ypos1 = 335 ; I, Q, U 
ypos2 = 325 ; V

y = ypos2
pmax = 0.0005;0.1
window, 1, xsize=nx,ysize=nf

tvscl,clip(s4[*,y,3,*],pmax,-pmax)
;tvscl,alog10(s4[*,y,0,*]>0<5000)
filename= path0 + "/he.V.pr.png"
WRITE_PNG,filename,TVRD(/TRUE)







END