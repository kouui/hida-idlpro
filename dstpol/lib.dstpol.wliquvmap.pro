
; history:
; 2022.06.03  k.u.  init
;********************************************************************
pro dispwliquvr,i,q,u,v,r,bin=bin,pmax=pmax,coms=coms,ialog=ialog,wid=wid,ipos=ipos,dd=dd, $
	isigma=isigma,title=title,wexist=wexist,yoffset=yoffset,ictour=ictour

if not keyword_set(bin) then bin=2
if not keyword_set(pmax) then pmax=0.01
if not keyword_set(coms) then coms=['I','Q','U','V','R']
if not keyword_set(dd) then dd=2
if not keyword_set(yoffset) then yoffset=0

;         ['white'  , 'green'  , 'red'    , 'yellow' , 'purple' , 'blue'   , 'brown'  , 'cyan'   ]
_colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x, '393893'x, 'd67e40'x, '206b5c'x, 'edf580'x] ; 'bbggrr'x

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
	if keyword_set(ictour) then begin
		nlevel=4
		colors = _colors[1:min([nlevel,n_elements(_colors)-1])]
		contour,alog(intens),position=[x0,dd+yoffset,x0+nx2-1,dd+yoffset+ny2-1],/device,nlevels=nlevel,c_colors=colors,/noerase,c_thick=0.2,xstyle=1+4,ystyle=1+4
	endif	
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
PRO wliquvmap, paths, dinfo, cal, prof_step=prof_step, selectwl=selectwl, pmax0=pmax0, bin=bin, savfolder=savfolder, ictour=ictour,ysiz=ysiz

if not keyword_set(pmax0) then pmax0 = 0.01
if not keyword_set(bin) then bin = 1
if not keyword_set(savfolder) then savfolder = 'sav/*.sav'

outdir = paths.workdir+paths.outdir
files = findfile(outdir+savfolder)
nf = n_elements(files)
if dinfo.nstep eq -1 then nstep = nf else nstep = dinfo.nstep
dd=2
restore,files[nstep/2],/RELAXED_STRUCTURE_ASSIGNMENT	; -> s[*,*,4],pcal
restore,cal.wl	; wl[]
imgsize,s,nx,ny,n4
;smap = fltarr(nx,nstep,4)
sa = fltarr(nx,ny,4,nstep)
loadct,0
for iii=0,nstep-1 do begin
	i = nstep - 1 - iii
	print,i,'  ',files[i]
	restore,files[i],/RELAXED_STRUCTURE_ASSIGNMENT
	if dinfo.div eq '' then pmax = pmax0 * max(s[*,*,0]) else pmax=pmax0
	;dispiquvr,s,bin=bin,pmax=pmax,wid=0,dd=dd ;,/ialog
	sa[*,*,*,i] = s
endfor

if not keyword_set(prof_step) then begin
restore,cal.flat	; fltl & fltr[nxp,ny], avfltl & avfltr[nxp,ny],fltspl & fltspr[nxp,ny]
imgsize,fltspl,nxp,nyp,n
iprof = fltspl[nxp/2,*,0] 
endif else begin
iprof = reform(rebin(sa[nx/2-3:nx/2+3,*,0,prof_step],1,ny,1,1))
endelse
if dinfo.wl_order eq 1  then iprof1=reverse(iprof) else iprof1=iprof


if keyword_set(selectwl) then begin
s = sa[*,*,*,dinfo.j_pos]
bin = binfact(s)
nx2 = nx/bin &	ny2 = ny/bin
if dinfo.div eq '' then pmax = pmax0 * max(s[*,*,0]) else pmax=pmax0
dispiquvr,s,bin=bin,pmax=pmax,wid=0,dd=dd ;,/ialog
print,'click an wavelength position'
cursor,px,py,3,/dev
x1 = (px-dd) mod (nx2+dd) &	j1 = (py-dd)
print,'click another wavelength position'
cursor,px,py,3,/dev
x2 = (px-dd) mod (nx2+dd) &	j2 = (py-dd)
ws = minmax([j1,j2] * bin)
w1 = ws[0] & w2 = ws[1]
endif else begin
w1 = 0 & w2 = ny-1
endelse

nscan = w2-w1+1
wid = 9 & dd = 2
nx3 = 300 &	ny3 = 300
if keyword_set(ysiz) then ny3 = ysiz
imgsize,s,nxp,ny,nn
nx2 = nxp/bin &	ny2 = ny/bin & nsp = nn
if dinfo.wl_order eq 1 then wla = reverse(wl) else wla = wl
;print, w1, w2

wliquvdir = outdir+'/wliquv/'
if not file_test(wliquvdir) then file_mkdir, wliquvdir
count = 0
nyplot = ny3
if ysiz gt 500 then nyplot /= 2
nyplot0 = 0.15
if ysiz gt 500 then nyplot0 /= 2
nywin = ny3+dd*2+(nyplot+dd)
window,wid,xs=nx3*nsp+dd*(nsp+1),ys=nywin
backg = uintarr(nx3*nsp+dd*(nsp+1),nywin)
position=[0.05,nyplot0,0.98,nyplot/float(nywin)]

for j0=w1,w2 do begin  ; loop over wavelength
	count += 1
	;if j0 ne 270 then continue
	if dinfo.wl_order eq 1 then j1=w2-(j0-w1) else j1=j0
	;print, j1
	; draw iquv images
	smap = fltarr(nx,nstep,4)
	for i=0,nstep-1 do smap[*,i,*] = sa[*,j1*bin,*,i]
	smap3 = congrid(smap,nx3,ny3,n4)
	if dinfo.div eq '' then pmax = pmax0 * max(smap3[*,*,0]) else pmax=pmax0
	;window,wid,xs=nx3*nsp+dd*(nsp+1),ys=ny3+dd*2+(ny3+dd)
    tvscl,backg
	dispwliquvr,smap3,bin=bin,pmax=pmax,wid=wid,/wexist,yoffset=nyplot+dd*2,coms=['I','Q','U','V'],ictour=ictour
	; draw i profile
    ;plot, wla, iprof1, position=[0.05,0.15,0.98,0.48], charsize=2,/noerase,/nodata
	plot, wla, iprof1, position=position, charsize=2,/noerase,$
		xr=[min(wla),max(wla)], yr=[0.95*min(iprof1),1.05*max(iprof1)],$
		xstyle=1,ystyle=1,xtickformat='(F7.0)',xtitle='wavelength [A]'
	wl0 = wl[j1]
	oplot, [wl0,wl0], [0.95*min(iprof1),1.05*max(iprof1)], line=2
	;stop
	; save plots
	fname=wliquvdir+string(count, format='%04d')+'.png'
	WRITE_PNG, fname, TVRD(/TRUE)
	print, 'saved as : ', fname
endfor ; end loop over wavelength

END
