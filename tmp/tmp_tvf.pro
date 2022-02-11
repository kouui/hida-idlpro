; tvf.pro
;  2020.6.27   k.i.
;  2020.8.5   k.i.
;  2021.8.10   k.i.   r keyword
pro tvf,img,x0,y0,prof=prof,imgr=imgr,r=r

if n_elements(img) eq 0 then begin
	print,'img not defined!'
	return
endif
if n_elements(x0) eq 0 then x0=0
if n_elements(y0) eq 0 then y0=0
imgsize,img,nx,ny
if !d.window eq -1 then window,xs=nx,ys=ny
wx=!d.x_size
wy=!d.y_size
rx=float(wx)/nx
ry=float(wy)/ny
r=min([rx,ry])
imgr=congrid(img,nx*r,ny*r)
tvscl,imgr,x0*r,y0*r
if keyword_set(prof) then profiles,imgr,factor=1./r

end

