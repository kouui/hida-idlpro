;********************************************************************
pro ku_dispiquvr,i,q,u,v,r,bin=bin,pmax=pmax,coms=coms,ialog=ialog,wid=wid,ipos=ipos,dd=dd, $
	isigma=isigma,title=title,wexist=wexist,yoffset=yoffset

if not keyword_set(bin) then bin=2
if not keyword_set(pmax) then pmax=0.01
if not keyword_set(coms) then coms=['I','Q','U','V','R']
if not keyword_set(dd) then dd=2
if not keyword_set(yoffset) then yoffset=0

imgsize,i,nxp,ny,nn
if nn ge 2 then begin
	nsp = nn
	ss = i
endif else begin
	ss = [[[i]],[[q]],[[u]],[[v]]]
	nsp=4
	if keyword_set(r) then begin
		ss = [[[ss]],[[r]]]
		nsp=5 
	endif
endelse	
nx2 = nxp/bin &	ny2 = ny/bin
if not keyword_set(wid) then wid=0

if keyword_set(wid) and keyword_set(wexist) then wset, wid $
else window,wid,xs=nx2*nsp+dd*(nsp+1),ys=ny2+dd*2,title=title

x0=dd

intens=congrid(ss[*,*,0],nx2,ny2)
if keyword_set(isigma) then sgm=isigma else sgm=[-3,1]
if keyword_set(ialog) then tvscls,alog(intens),x0,dd+yoffset $
else tvscls,intens,x0,dd+yoffset,sgm=sgm ;vmax=median(ss[*,*,0])*1.2
if n_elements(coms) eq nsp then xyouts,10,ny2-30+yoffset,coms[0],chars=3,charthick=2,/dev,color='9900ff'x
for j=1,nsp-1 do begin
	x0 = dd+(nx2+dd)*j
	tvscls,congrid(ss[*,*,j],nx2,ny2),x0,dd+yoffset,vmin=-pmax,vmax=pmax
	if n_elements(coms) ge nsp then begin
		xyouts,x0+10,ny2-30+yoffset,coms[j],chars=3,charthick=2,/dev,color='9900ff'x
	endif
endfor
if n_elements(ipos) ne 0 then begin
	for j=0,nsp-1 do begin
		x0 = dd+(nx2+dd)*j
		draw,x0+ipos/bin*[1,1],dd+[0,ny2]+yoffset,line=1
	endfor
endif

end
