;+
; NAME       : mkspflat (function)
; PURPOSE    :
;	make flat for spectra
; CATEGORY :
;	idlpro/data_red
; CALLING SEQUENCE :
;       function mkspflat(flat_avg,x00,xw=xw,xf=xf,xc=xc,yrange=yrange,delx=delx_a, $
;	prms=prms)
; INPUTS :
; 	flat_avg  -- average spectrum
;	x00	  -- absorption line pos.
; OPTIONAL INPUT PARAMETERS : 
;	none
; KEYWORD PARAMETERS :
;	xw	-- interval for searching an absorption line, (20)
; 	xf 	-- fitting width xf*2+1 for line minimum, (7)
;	xc[2]   -- continuum interval, [0,100]
;	yrange[2] -- processing data range along slit
;	prms	-- [xw,xf,xc,yrange] structure
; OUTPUTS :
;	spectrum flat
;	delx[*]	-- return line position along slit
; COMMON BLOCKS : 	none
; SIDE EFFECTS :
; RESTRICTIONS :	slit must be in y-direction
; MODIFICATION HISTORY :
;	2018.12.05 	c4
;	2020.06.22	ki	prms keyword
;	2020.08.23	ki	bug fix
;	2021.05.17	ki	xshift_sbsp
;	2022.03.01	ki	vertical_disp keyword
;	2022.03.18	ki	recover linear trend along wl
;-

;
function mkspflat,flat_avg0,x00,xw=xw,xf=xf,xc=xc,yrange=yrange,delx=delx_a,prms=prms,vertical_disp=vert

if keyword_set(vert) then flat_avg=rotate(flat_avg0,3) else flat_avg=flat_avg0
; size decision
s = size(flat_avg)
nx = s[1]
ny = s[2]

if keyword_set(prms) then begin
	x00=prms.x00
	if prms.xw ne 0 then xw=prms.xw
	if prms.xf ne 0 then xf=prms.xf
	if prms.xc[0] ne -1 then xc=prms.xc
	if prms.yrange[0] ne -1 then yrange=prms.yrange
endif
if not keyword_set(xw) then xw = 20
if not keyword_set(xf) then xf = 7
if not keyword_set(xc) then xc = [0,100]

if keyword_set(yrange) then begin
   ystart = yrange[0]
   yend = yrange[1]
endif else begin
   ystart = 0
   yend = ny - 1
endelse
if keyword_set(prms) then begin
	prms.yrange=[ystart,yend]
	prms.xw=xw
	prms.xf=xf
	prms.xc=xc
endif


; find minimum of absorption line
; 2Dfitc

nslit = yend - ystart + 1
min_pos = fltarr(nslit)
min_val = fltarr(nslit)
xx = findgen(2*xf+1)
for i = ystart, yend do begin
   prof = flat_avg[x00-xw:x00+xw,i]
   imin = min(prof,x0)
   icor = prof[x0-xf:x0+xf]
   result0 = poly_fit(xx,icor,2)
   ;y=a+bx+cx^2
   min_pos[i-ystart] = -result0[1]/(2*result0[2])+x0
   min_val[i-ystart] = -(result0[1]^2-4*result0[2]*result0[0])/(4*result0[2])
endfor


; poly_fit
y1 = dindgen(yend-ystart+1) + ystart
x1 = min_pos
result1 = poly_fit(y1,x1,4)
;xfit1 = result1[0] + result1[1]*y1 + result1[2]*(y1^2)
xfit1 = poly(y1,result1)
sa = xfit1 - min_pos
d = where(abs(sa) lt 1.0)

; poly_fit2
x2 = x1[d]
y2 = y1[d]
result2 = poly_fit(y2,x2,4)
;xfit2 = result2[0] + result2[1]*y1 + result2[2]*(y1^2)
xfit2 = poly(y1,result2)

; make vertical obsorption line
flat_vert = fltarr(nx,ny)
x_ref = mean(xfit2)
delx_a = fltarr(ny)
for i = ystart, yend do begin
   delx = x_ref - xfit2[i-ystart]
   delx_a[i] = delx
   dely = 0
   prof_origin = flat_avg[*,i]
   kf0 = fft(prof_origin,-1)
   kf1 = xshift_sbsp( kf0, delx )
   prof_new = fft(kf1,1)
   ;prof_new = fshift(reform(prof_origin,nx,1), delx, dely)
   flat_vert[*,i] = reform(prof_new, nx)
endfor

; average over slit
flat_slit = total(flat_vert[*,ystart:yend], 2) / (yend-ystart+1)
; expand to 1600 x 1200
flat_exp = fltarr(nx,ny)
for i=0,ny-1 do begin
   kf0 = fft(reform(flat_slit,nx,1),-1)
   kf1 = xshift_sbsp( kf0, -delx_a[i] )
   flat_exp[*,i] = fft(kf1,1)
   ;flat_exp[*,i] = fshift(reform(flat_slit,nx,1), -delx_a[i], dely)
endfor

if keyword_set(xc) then begin ; continuum level and correction of variable line depth (iscat)

   icont = total(flat_vert[xc[0]:xc[1],ystart:yend],1) / (xc[1]-xc[0]+1.)

;icont fit
   dep3 = min_val/icont
   result3 = poly_fit(y1,dep3,6)
   fit3 = poly(y1,result3)

   sa3 = fit3 - dep3
   d3 = where(abs(sa3) lt 0.005)

   dep4 = dep3[d3]
   y4 = y1[d3]
   result4 = poly_fit(y4,dep4,6)
   fit4 = poly(y1,result4)

   min_val_avg = mean(min_val)
   icont_avg = mean(icont)
   iscat = (fit4*icont_avg - min_val_avg) / (1-fit4)

   for i=ystart,yend do begin   
      flat_exp[*,i] = (flat_exp[*,i]+iscat[i-ystart])*icont_avg / (icont_avg+iscat[i-ystart])
   endfor

endif

; flat field division
flat_field = flat_avg / flat_exp

; recover linear trend in wl
a = double(flat_avg[*,ystart:yend])
a = a/mean(a)
coeffs = dblarr(2,nslit)
x = findgen(nx)
for i=0,nslit-1 do begin
	a1 = a[*,i]
	for k=0,7 do begin
		coeff = poly_fit(x,a1,1,yfit=b1)
		ii = where(a1 lt b1)
		a1[ii] = b1[ii]
	endfor
	;plot,a[*,i] &	oplot,a1
	coeffs[*,i] = coeff
endfor
i0 = transpose(coeffs[0,*])
didx = transpose(coeffs[1,*])
y = dindgen(nslit)
dcut = [0.3,0.2,0.1,0.05,0.02,0.005]

;; if where no found, ii=-1, seems no problem?  2022.05.20, kouui
;; added `if ii[0] ne -1` statement
for k=0,n_elements(dcut)-1 do begin
	dmy = poly_fit(y,i0,3,yfit=c1)
	ii = where(abs(i0-c1) gt dcut[k])
	if ii[0] ne -1 then i0[ii] = c1[ii]
	dmy = poly_fit(y,didx,3,yfit=d1)
	ii = where(abs(didx-d1) gt dcut[k]*median(didx))
	if ii[0] ne -1 then didx[ii] = d1[ii]
endfor
for i=0,nslit-1 do begin
	rx = i0[i]+ didx[i]*x
	flat_field[*,ystart+i] = flat_field[*,ystart+i]*rx
	;plot,a[*,i],yr=[0,2] &	oplot,rx &	wait,0.05
endfor

;dcut = [0.3,0.2,0.1,0.05,0.02,0.005]
;kfit = 1
;for i=0,n_elements(dcut)-1 do begin
;	b = sfit(a,kfit)
;	ii = where(abs(a-b) gt dcut[i])
;	a[ii] = b[ii]
;endfor
;b = sfit(a,kfit)
;flat_field[*,ystart:yend] = flat_field[*,ystart:yend]*b

if ystart gt 0 then flat_field[*,0:ystart-1]=1.
if yend lt ny-1 then flat_field[*,yend+1:ny-1]=1.

if keyword_set(vert) then flat_field=rotate(flat_field,1)


return,flat_field

end
