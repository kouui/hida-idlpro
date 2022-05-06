;  dst_pollib
;   2022.04.12  u.k.    copy from /home/ichimoto/idlpro/hida/dstpol/dst_pollib.pro 
;   2022.04.12  u.k.    added wexist keyword to dispiquvr
;   2022.05.03  u.k.    added ku_mk_drkflt, skip flat processing if flat_org exist
;********************************************************************
pro dispwliquvr,i,q,u,v,r,bin=bin,pmax=pmax,coms=coms,ialog=ialog,wid=wid,ipos=ipos,dd=dd, $
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

;********************************************************************
pro ku_mk_drkflt, darkfiles,flatfiles,drk,avflt, flatdark=flatdark,savfile=savfile,hd=hd,hf=hf
; make dark & flat average  ->  drk[nx,ny],  avflt[nx,ny,nn]

	drks = read_dstfits(darkfiles[0],hd,cam=cam,dst=dst,tim=tim)
	imgsize,drks,nx,ny,nnd
	drks = rm_badframe(drks)
	drk = rebin(drks,nx,ny,1)
	if file_test(savfile.flat_org) then begin
		save,drk,hd,file=savfile.dark
		print,'dark saved in ',savfile.dark
		print,'flat processing skipped : found ', savfile.flat_org
		return
	endif
	nflt = n_elements(flatfiles)
	flt1 = read_dstfits(flatfiles[0],hf,cam=cam,dst=dst,tim=tim)
	imgsize,flt1,nx,ny,nnf
	avflt = float(flt1)
	count=0
	for i=1,nflt-1 do begin
		print,'flat-',string(i+1,nflt,form='(i3,"/",i3)'),'  ',flatfiles[i]
		flt1 = read_dstfits(flatfiles[i],hf,cam=cam,dst=dst,tim=tim)
		if min(flt1) gt 0 then begin
			avflt = avflt+flt1
			count = count+1
		endif else print,'Skip a bad flat..'
	endfor
	avflt = avflt/count
	if keyword_set(flatdark) then begin
		print,'flatdark..  ',flatdark[0]
		fdrks = read_dstfits(flatdark[0],hd,cam=cam,dst=dst,tim=tim)
		fdrks = rm_badframe(fdrks)
		fdrk = rebin(fdrks,nx,ny,1)
	endif else fdrk = drk
	for j=0,nnf-1 do avflt[*,*,j] = avflt[*,*,j]-fdrk
	if keyword_set(savfile) then begin
		save,drk,hd,file=savfile.dark
		print,'dark saved in ',savfile.dark
		save,avflt,hf,file=savfile.flat_org
		print,'flat saved in ',savfile.flat_org
	endif

end

