; x0fit
;  fit absorption line center
;	'95/06/09	k.i.
;	2019.11.14	k.i.  bug dx offset
;	2020.02.21	k.i.  if k=2  => -b/2a
;	2020.07.04	k.i.  return z0 as keyword

function x0fit,sp,ix00,dx,k=k,z0=z0

if not keyword_set(k) then k=2

s=size(sp)	&	nx=s[1]	&	ny=s[2]
if s[0] eq 1 then ny=1
x0=dblarr(ny)
z0=dblarr(ny)
ii=indgen(dx*2+1)
for j=0,ny-1 do begin
	sp1=sp[*,j]
	wx=3*dx
	spmin=min(sp1[ix00-wx:ix00+wx],ix0)
	ix0=ix00+ix0-wx
	coeff=poly_fit(ii,sp1[ix0-dx:ix0+dx],k)
	;spfit=poly(ii,coeff)
	;plot,sp1(ix0-dx:ix0+dx) &	oplot,spfit,line=1
	;ans='' &	read,ans
	if k eq 2 then begin
		xo=-coeff[1]/2./coeff[2]
		x0[j]=ix0+xo-dx
		z0[j]=coeff[2]*xo^2 + coeff[1]*xo + coeff[0]
		;print,x0[j]
	endif else begin
		dcoeff=findgen(k+1)*coeff &	dcoeff=dcoeff[1:k]
		zroots,dcoeff,xx
		dxmin=min(abs(xx-ix0),i)
		x0[j]=ix0+xx[i]-dx
		z0[i]=poly(x0[i],coeff)
	endelse
	ix00=ix0
endfor

return,x0
end
