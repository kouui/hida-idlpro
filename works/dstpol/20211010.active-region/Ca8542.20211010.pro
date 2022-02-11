; dstpol_20211010.pro
@mmdst
@dst_pollib

;---------------------------------------------------------------------------
dir0 = '/mnt/HDD3TBn47/DST/sp/20211010/'		; root directory of raw dataset
caldir = 'cal/'						; holder for clib. data
setdir = 'ar1/' 					;    "   for obs data
jf = 29 & ipos = 220 & pmax = 0.02 			; spot obs.data
workdir = '/nwork/kouui/data-lvl1/dstpol/20211010.active-region/'		; output directory in /nwork
outdir=setdir
wl0 = 8542. &	wl_range = 15.				; central wavelength & range [A] 	
wl_order = 1						; 0: top is red, 1: top is blue
darkfiles = findfile(dir0+caldir+'cadrk*.fits') 	; dark
flatfiles = findfile(dir0+caldir+'ca*.fits')		; flat
flatdark = ''						; dark for flat when expo. is deferent 
hairline = findfile(dir0+caldir+'cahl*.fits')		; heirline
polarizer = findfile(dir0+caldir+'capol*.fits')		; polarizer
files = findfile(dir0+setdir+'*.fits')			; obs. data
avdfsav = workdir+'cal/avdforig_Ca.sav'			; averaged dark & flat
calparsav = workdir+'cal/calparam_Ca.sav'		; cp structure
drkfltsav = workdir+'cal/drkflt_Ca.sav'			; drk[nx,ny] & fltl,fltr[nxp,ny,nn]
wlsav = workdir+'cal/wl_Ca.sav'
nstep = 60						; # of step for 1 scan
;th_offset = -41.5		;			; WP offset in deg.
th_offset = 46.						; WP offset in deg.
telpos='WEST'						; telescope position
;---------------------------------------------------------------------------


step = 5
case step of
	0: goto,drkflt		; average dark & flat, save in workdir
	1: goto,setparms	; manual setting of alignment parameters using hairline spectrum
	2: goto,mkflat		; make separate flats for two spectra, savel in workdir
	3: goto,demoparam	; determine demodulation params. del, trhoughput ratio using pol.data
	4: goto,qlmap		; make 2D QL map for scan obs.
	5: goto,check		; check alignmenyt, flat & demodulation result
	6: goto,obscal		; calib. obs.seq., save results in workdir
	7: goto, wlident	; wave length identification
	8: goto, pltprof	; plot iquv profiles at ipos
endcase



drkflt: 
;***** make dark & flat average *******************************
print,'making dark & flat average ....'
mk_drkflt, darkfiles,flatfiles,drk,avflt, flatdark=flatdark,hd=hd,hf=hf ,savfile=avdfsav

stop

setparms:
;*****  set params for dual-beam alignment   ***************
print,'read ',avdfsav
restore,avdfsav  ; => drk[nx,ny], avflt[nx,ny,nn], hd, hf
hlsp = read_dstfits(hairline[0],hh,cam=cam,dst=dst,tim=tim)
imgsize,hlsp,nx,ny,nn
hlsp=rebin(hlsp,nx,ny,1)-drk

dualsp_align,hlsp,cp,spl,spr,/measure	; 
cp.datinfo = fits_keyval(hf,'DATE_OBS')
save,cp,file = calparsav
print,'new cp.* saved in ',calparsav

stop

mkflat:
;*****  make separate flat  ******
restore,avdfsav  ; => drk[nx,ny], avflt[nx,ny,nn], hd, hf
restore,calparsav ; => cp
imgsize,avflt,nxp,ny,nn
nxp = cp.ix2-cp.ix1+1
print,'Making fltl[*,*,nn], fltr[*,*,nn]'
fltl = fltarr(nxp,ny,nn)  &	fltr = fltl
for i=0,nn-1 do begin
	print,i
	dualsp_align,avflt[*,*,i],cp,flt1l,flt1r
	flt1l = flt1l/rebin(rebin(flt1l,1,ny),nxp,ny)
	flt1r = flt1r/rebin(rebin(flt1r,1,ny),nxp,ny)
	fltl[*,*,i] = flt1l
	fltr[*,*,i] = flt1r
endfor
;-- self-flat correction for check --
flt1l = rebin(fltl,nxp,ny,1)
flt1r = rebin(fltr,nxp,ny,1)

save,drk,fltl,fltr,flt1l,flt1r,file = drkfltsav
print,'calib.params saved in  '+ drkfltsav
stop

demoparam:
;*****  demod. param. *****************************
restore,calparsav
spp = read_dstfits(polarizer[0],hp,cam=cam,dst=dst,tim=tim)
imgsize,spp,nx,ny,nn
rotp = fits_keyval(hp,'ROTP',/fix)
expo = fits_keyval(hp,'EXP',/float)
nrot = expo*nn/rotp	; # of waveplate rotation in a set
mprof1 =  reform(rebin(spp[cp.ix1:cp.ix2,cp.yc[0]:cp.yc[1],*],1,1,nn),nn)
mprof2 =  reform(rebin(spp[cp.ix1+cp.ddx:cp.ix2+cp.ddx, $
			   cp.yc[0]+cp.ddy:cp.yc[1]+cp.ddy,*],1,1,nn),nn)
th = findgen(nn)/nn * nrot * 2*!pi
; DAT = AV + AMP * sin(K * XX + PH)
fit1 = ta_sinfit_mpfit(th,mprof1,av=av1,amp=amp1,k=k1,ph=ph1) 
fit2 = ta_sinfit_mpfit(th,mprof2,av=av2,amp=amp2,k=k2,ph=ph2) 
amp1 = abs(amp1) &	amp2 = abs(amp2)
rlr = amp2/amp1
mprof = mprof1+mprof2/rlr
av = mean(mprof)/2 &	amp = amp1+amp2/rlr
del = acos(1-amp/av)/!pi*180

;th_offset = atan(ss,cc)/!pi*180. - 45.	; refer polarizer axis,  not work well
cp.nrot = nrot
cp.th_offset = th_offset
cp.telpos = telpos	; DST position 'WEST' or 'EAST'
cp.rlr = rlr	; trhoughput ratio r/l
cp.del = del	; retardation of wp in deg.
cp.wl_order = wl_order	; 0: top is red, 1: top is blue  -> reft sp. is +U or -U
;if cp.wl_order eq 1 then cp.rotang = cp.rotang + 90.
;cp.rotang = -45.

save,cp,file = calparsav
print,'demod.params: nrot= ',cp.nrot,',   th_offset=',cp.th_offset
print,'new cp.* saved in ',calparsav

stop

qlmap:
;*****  QL map *****************
;--- map ---
paramfile = findfile(calparsav, count=paramok)
if paramok then restore,paramfile[0]  ;  -> cp
nf = n_elements(files)
sp1 = read_dstfits(files[0],h,nslice=1)
imgsize,sp1,nx,ny
sps = fltarr(nx,ny,nstep)
nxp = cp.ix2-cp.ix1+1
limg = fltarr(nxp,nstep)
cimg = fltarr(nxp,nstep)
j0 = nstep*0

for j=0,nstep-1 do begin
	sp1 = read_dstfits(files[j0+j],h,nslice=1)
	limg[*,j] = sp1[cp.ix1:cp.ix2,cp.yl_l[0]]
	cimg[*,j] = sp1[cp.ix1:cp.ix2,cp.yc[0]]
endfor
tvf,congrid(cimg,200,200)
;spos = fltarr(nf)
;hs = gt_fitsheads(files)
;for i=0,nf-1 do spos[i] = fits_keyval(hs[*,i],'S4POS',/float)

stop

check:
;***** check calibration result *************************** 
restore,calparsav
restore,drkfltsav	; cp, drk[nx,ny], fltl[nxp,ny,nn], fltr[nxp,ny,nn]
imgsize,fltl,nxp,ny,nnc
fltl= rebin(fltl,nxp,ny,1) &	fltr= rebin(fltr,nxp,ny,1)  ; <-- average all flats
if nxp gt 500 then bin = 3 
if nxp gt 300 and nxp lt 500 then bin = 2
if nxp lt 300 then bin = 1
nf=n_elements(files)

j0 = nstep*0
cp.th_offset = th_offset
file1 = files[j0+jf]
;file1 = polarizer[0]
print,jf,'  reading ',file1
sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)
dualsp_calib1,sps,drk,fltl,fltr,cp,spsl,spsr;,/ver	; dark, two-sp, aligh, flat
s = dualsp_demodulate(spsl,spsr,cp,sl=sl,sr=sr,div='c') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
dispiquvr,s,bin=bin,pmax=pmax,coms=['I','Q','U','V','R'],/ialog
s2 = correct_Icrosstk(s)
dispiquvr,s2,bin=bin,pmax=pmax,coms=['I','Q','U','V','R'],/ialog,wid=2

;--- correct DST polarization ---
mm = get_dstmm(wl0,telpos,dst,sc=sc,qin='slit')
rmm = invert(mm)
;rmm[0,2]=-rmm[0,2]
s3 = s[*,*,0:3]
for j=0,3 do begin
	s3[*,*,j] = rmm[0,j]*s[*,*,0]
	for i=1,3 do s3[*,*,j] = s3[*,*,j] + rmm[i,j]*s[*,*,i]
endfor
;s3 = correct_Icrosstk(s3)
dispiquvr,s3,bin=bin,pmax=pmax,coms=['I','Q','U','V'],/ialog,wid=3 ;


stop
obscal:
;***** calib obs. sequence data *************************** 
restore,calparsav
restore,drkfltsav	; cp, drk[nx,ny], fltl[nxp,ny,nn], fltr[nxp,ny,nn]
imgsize,fltl,nxp,ny,nn
fltl= rebin(fltl,nxp,ny,1) &	fltr= rebin(fltr,nxp,ny,1)  ; <-- average all flats
if nxp gt 500 then bin = 3 
if nxp gt 300 and nxp lt 500 then bin = 2
if nxp lt 300 then bin = 1

nf=n_elements(files)
print,'obscal: processing '+string(nstep,form='(i3)')+' files'
for j=0,nstep-1 do begin
	file1=files[j0+j]
	print,j,'  reading ',file1
	;sps = float(uint(readfits(file1,h)))
	sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)

	cp.expo = fits_keyval(h,'EXP',/float)
	dualsp_calib1,sps,drk,fltl,fltr,cp,spsl,spsr;,/ver	; dark, two-sp, aligh, flat

	demod:
	;***** demodulation and cocmbine left & right sp  *****
	s = dualsp_demodulate(spsl,spsr,cp,sl=sl,sr=sr);,div='i') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]

	filename_sep,file1,di,fnam,ex
	dispiquvr,s[*,*,[0:4]],bin=bin,pmax=pmax,coms=['I','Q','U','V','R'],/ialog
	xyouts,10,10,string(j,form='(i3)')+': '+fnam,/dev,chars=3,col=0
	save,s,cp,h,file=workdir+outdir+'iquv/'+fnam+'.sav'
	win2gif,workdir+outdir+'iquvimg/'+fnam+'.gif'
endfor

stop

wlident:
;***** wavelength identification *************************** 
imgsize,s,nxp,nyp,n

iprof = transpose(s[nxp/2,*,0]) 
if cp.wl_order eq 1  then iprof1=reverse(iprof) else iprof1=iprof
wl = wlident(iprof1,wl0,wl_range)
if cp.wl_order eq 1 then wl=reverse(wl)
save,iprof,wl,file=wlsav

pltprof:
;***************  plot profiles *****************
;savfiles=findfile(workdir+outdir+'iquv/*.sav')
;restore,savfiles[jf]	;-> s[*,*,*]
restore,wlsav	;-> wl,iprof
iprof0 = iprof
bin=1
imgsize,s,nxp,nyp,n5
nxp2=nxp/bin &	nyp2=nyp/bin &	ix=ipos/bin
sb = congrid(s2,nxp2,nyp2,5)
iprof = transpose(sb[ix,*,0])
qprof = transpose(sb[ix,*,1])	
uprof = transpose(sb[ix,*,2])	
vprof = transpose(sb[ix,*,3])	
if wreverse then begin	; reverse wavelength
	wl=reverse(wl)
	iprof=reverse(iprof)
	qprof=reverse(qprof)
	uprof=reverse(uprof)
	vprof=reverse(vprof)
endif


window,xs=1200,ys=850
stretch,255,0
wy1=0.20
box=[0.1,0.,.95,wy1]
yoff=[0,1,0,1]
filename_sep,files[jf],di,fnam,ex
blank=replicate(' ',10)
yr=0.01*[-1,1]
wln=wl/10
xr=[wln[0],wln[nyp2-1]]
cs=2.2
title=fnam+'  x='+string(ipos,form='(i3)')
plot,wln,iprof,pos=box+yoff*(0.1+wy1*3),/norm,xtickname=blank,chars=cs,xr=xr,xstyle=1,yr=[0,1.1],ystyle=1,title=title
xyouts,wln[20],1.,'I',/data,chars=2.5
plot,wln,qprof,pos=box+yoff*(0.1+wy1*2),/norm,xtickname=blank,/noerase,chars=cs,xr=xr,xstyle=1,yr=yr,ystyle=1
oplot,xr,[0,0],line=1
xyouts,wln[20],yr[1]*0.6,'Q/Ic',/data,chars=2.5
plot,wln,uprof,pos=box+yoff*(0.1+wy1*1),/norm,xtickname=blank,/noerase,chars=cs,xr=xr,xstyle=1,yr=yr,ystyle=1
oplot,xr,[0,0],line=1
xyouts,wln[20],yr[1]*0.6,'U/Ic',/data,chars=2.5
plot,wln,vprof,pos=box+yoff*(0.1+wy1*0),/norm,/noerase,xtitle='wavelength [nm]',chars=cs,xr=xr,xstyle=1,yr=yr,ystyle=1
oplot,xr,[0,0],line=1
xyouts,wln[20],yr[1]*0.6,'V/Ic',/data,chars=2.5

;win2gif,workdir+outdir+'profs.gif'

window,2,xs=nxp*4,ys=nyp
loadct,0
for i=0,3 do begin
	tvscl,sb[*,*,i],nxp*i,0
	draw,ix*[1,1]+nxp*i,[0,nyp]
endfor

stop


end

