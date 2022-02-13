; dstpol_20220209_NaD_limb.pro

@mmdst
@mmdst_lib
@dst_pollib

;---------------------------------------------------------------------------
workdir = '/nwork/kouui/data-lvl1/dstpol/20220211.active-region/spec/'		; output directory in /nwork
;	dir0 = '/mnt/HDD3TBn51/DST/sp/20220209/spec/'	; root directory of raw dataset
;	caldir = 'NaI5896/calib/'					; holder for clib. data
;	setdir = 'NaI5896/ar2/'  & nstep = 9 & jf = 3 		 	;    "   for obs data, # of step for 1 scan
;	;setdir = 'NaI5896/limb1/'  & nstep = 30	 & jf = 10 		;    "   for obs data, # of step for 1 scan
;	outdir='NaD_limb/'
;	wl0 = 5893. &	wl_range = 15.				; central wavelength & range [A] 	
;	ipos = 60 & pmax = 0.01			; slit pos.,  wl pos.,  
;	darkfiles = findfile(dir0+'dark/dark_orca_50ms_*.fits') 	; dark
;	flatdark = ''						; dark for flat when expo. is deferent 
;	avdfsav = workdir+'cal/avdforig_Na.sav'			; averaged dark & flat
;	calparsav = workdir+'cal/calparam_Na.sav'		; cp structure
;	drkfltsav = workdir+'cal/drkflt_Na.sav'			; drk[nx,ny] & fltl,fltr[nxp,ny,nn]
;	wlsav = workdir+'cal/wl_Na.sav'

dir0 = '/mnt/HDD3TBn51/DST/sp/20220211/spec/OI7772/'
caldir = '/calib/'
obsdir = '/ar3/'
;obsdir = 'NaI5896/limb1/'
path = {dstpol_path, $	
	rootdir: dir0, $	; root directory of raw dataset
	obsdir: obsdir,					$	; holder of obs dataset
	caldir: caldir,					$ 	; holder of clib. data
	darkfile: dir0+'/calib/dark_30ms_*.fits',	$ 	; dark file path in caldir
	flatfile: dir0+caldir+'/flat*.fits',		$ 	; flat file path in caldir
	flatdark: '',					$	; dark for flat in caldir if expo. is different 
	hairline: dir0+caldir+'/hair_*.fits',		$	; hairline data in caldir 
	polarizer: dir0+caldir+'/pol_*.fits',		$	; polarizer data in caldir
	obsdat: dir0+obsdir+'/orca*.fits',			$	; output root directory in /nwork
	workdir: workdir  ,	$	; output root directory in /nwork
	outdir: obsdir,				$	; directory for obs.data output in workdir
	calpdir: caldir,				$	; directory for saving calib.params in workdir
	avdfsav: 'avdforig.sav',			$	; averaged dark & flat
	calparsav: 'calparam.sav',			$	; cp, ap, dinfo, path structures
	drkfltsav: 'drkflt.sav',			$	; drk[nx,ny] & fltl,fltr[nxp,nyp]
	wlsav: 'wl.sav'				$	; wl[*] save file
	}
dinfo = {dstpol_obs, $	; 
	nstep: 80 ,		$ ; # of steps for 1 scan
	i_scan: 0 ,		$ ; scan # for testing cal.
	j_pos:  55 ,		$ ; position # in a scan for test
	wl0: 7772 ,		$ ; entral wavelength [A]
	wl_range: 15.,		$ ; apporox. wavelength range [A] 	
	wl_order: 1.,		$ ; 0: top is red, 1: top is blue  -> left sp is +U or -U
	telpos:	'EAST',		$ ; DST position,  'WEST' or 'EAST',  get from dst_info
	incli: 44./60.,		$ ; inclination if dst_info not available
	expo:	0.03,		$ ; exposure time, sec 
	nrot:	1.,		$ ; # of wp rotation during accuisition
	th_offset: 40.,		$ ; offset angle of waveplate to eliminate R, deg.
	rotang:	45.,		$ ; angle between PBS ax and slit to make +Q perpendicular to slit
	rlr: 	0.,		$ ; throughtput ratio,  amp_r/amp_l
	del: 	0.		$ ; retardation of WP in deg.
	}


;path.dir0
;path.caldir
;pmax = 0.01			; slit pos.,  wl pos.,  
pmax = 0.02	 ; max p for display

undefine,rfiles
rfiles = {read_files,$
	dark: findfile(path.darkfile), $	; dark
	flat: findfile(path.flatfile), $	; flat
	hairline: findfile(path.hairline), $ ;hairline
	polarizer: findfile(path.polarizer), $	; polarizer
	files: findfile(path.rootdir+path.obsdir+'*.fits') $	; obs. data
	}
calpdir = path.workdir+path.calpdir
undefine,savfiles
savfiles = {save_files, $
	avdf: calpdir+path.avdfsav, $		; averaged dark & flat
	calpar: calpdir+path.calparsav, $	; cp structure
	drkflt: calpdir+path.drkfltsav, $	; drk[nx,ny] & fltl,fltr[nxp,ny,nn]
	wl: calpdir+path.wlsav $ 		; wl[*] save file
	}
;---------------------------------------------------------------------------

menu = ['0: drkflt', $		; average dark & flat, save in workdir
	'1: setparms', $ 	; manual setting of alignment parameters using hairline spectrum
	'2: mkflat', $		; make separate flats for two spectra, save in workdir
	'3: demoparam', $	; determine demodulation params. del, trhoughput ratio using pol.data
	'4: qlmap', $		; make 2D QL map for scan obs.
	'5: check', $		; check alignmenyt, flat & demodulation result
	'6: obscal', $		; calib. obs.seq., save results in workdir
	'7: wlident', $		; wave length identification
	'8: pltprof', $		; plot iquv profiles at ipos
	'cancel', $
	'debug' $
	]

;step = smenu(menu)
step = 5
case step of
	0: goto,drkflt		; average dark & flat, save in workdir
	1: goto,setparms	; manual setting of alignment parameters using hairline spectrum
	2: goto,mkflat		; make separate flats for two spectra, save in workdir
	3: goto,demoparam	; determine demodulation params. del, trhoughput ratio using pol.data
	4: goto,qlmap		; make 2D QL map for scan obs.
	5: goto,check		; check alignmenyt, flat & demodulation result
	6: goto,obscal		; calib. obs.seq., save results in workdir
	7: goto,wlident		; wave length identification
	8: goto, pltprof	; plot iquv profiles at ipos
	9: stop
	10: goto,debug
endcase



drkflt: 
;***** make dark & flat average *******************************
print,'making dark & flat average ....'
mk_drkflt, rfiles.dark,rfiles.flat,drk,avflt, flatdark=path.flatdark,hd=hd,hf=hf ,savfile=savfiles.avdf

stop

setparms:
;*****  set params for dual-beam alignment   ***************
print,'read ',savfiles.avdf
restore,savfiles.avdf  ; => drk[nx,ny], avflt[nx,ny,nn], hd, hf
hlsp = read_dstfits(rfiles.hairline[0],hh,cam=cam,dst=dst,tim=tim)
imgsize,hlsp,nx,ny,nn
hlsp=rebin(hlsp,nx,ny,1)-drk

dualsp_align,hlsp,ap,spl,spr,/measure	; 
ap.filename = rfiles.hairline[0]
save,ap,file = savfiles.calpar
print,'new calparms.* saved in ',savfiles.calpar

stop

mkflat:
;*****  make separate flat  ******
restore,savfiles.avdf  ; => drk[nx,ny], avflt[nx,ny,nn], hd, hf
restore,savfiles.calpar ; => ap, dinfo
imgsize,avflt,nxp,ny,nn
nxp = ap.ix2-ap.ix1+1
print,'Making fltl[*,*,nn], fltr[*,*,nn]'
fltl = fltarr(nxp,ny,nn)  &	fltr = fltl
for i=0,nn-1 do begin
	print,i
	dualsp_align,avflt[*,*,i],ap,flt1l,flt1r
	flt1l = flt1l/rebin(rebin(flt1l,1,ny),nxp,ny)
	flt1r = flt1r/rebin(rebin(flt1r,1,ny),nxp,ny)
	fltl[*,*,i] = flt1l
	fltr[*,*,i] = flt1r
endfor
;-- self-flat correction for check --
flt1l = rebin(fltl,nxp,ny,1)
flt1r = rebin(fltr,nxp,ny,1)

;save,drk,fltl,fltr,flt1l,flt1r,file = savfiles.drkflt
fltl = flt1l
fltr = flt1r
save,drk,fltl,fltr,file = savfiles.drkflt
print,'dark and separate flats saved in  '+ savfiles.drkflt
stop

demoparam:
;*****  demod. param. *****************************
restore,savfiles.calpar
spp = read_dstfits(rfiles.polarizer[0],hp,cam=cam,dst=dst,tim=tim)
imgsize,spp,nx,ny,nn
rotp = fits_keyval(hp,'ROTP',/fix)
expo = fits_keyval(hp,'EXP',/float)
dinfo.nrot = expo*nn/rotp	; # of waveplate rotation in a set
mprof1 =  reform(rebin(spp[ap.ix1:ap.ix2,ap.yc[0]:ap.yc[1],*],1,1,nn),nn)
mprof2 =  reform(rebin(spp[ap.ix1+ap.ddx:ap.ix2+ap.ddx, $
			   ap.yc[0]+ap.ddy:ap.yc[1]+ap.ddy,*],1,1,nn),nn)
th = findgen(nn)/nn * dinfo.nrot * 2*!pi
; DAT = AV + AMP * sin(K * XX + PH)
fit1 = ta_sinfit_mpfit(th,mprof1,av=av1,amp=amp1,k=k1,ph=ph1) 
fit2 = ta_sinfit_mpfit(th,mprof2,av=av2,amp=amp2,k=k2,ph=ph2) 
if amp1 lt 0 then begin &	amp1 = -amp1 &	ph1 = ph1+!pi &	endif
if amp2 lt 0 then begin &	amp2 = -amp2 &	ph2 = ph2+!pi &	endif
;amp1 = abs(amp1) &	amp2 = abs(amp2)
rlr = amp2/amp1	
mprof = mprof1+mprof2/rlr
av = mean(mprof)/2 &	amp = amp1+amp2/rlr
dinfo.del = acos(1-amp/av)/!pi*180	; retardation of wp in deg.
dinfo.rlr = rlr		; trhoughput ratio r/l

;th_offset_ = atan(ss,cc)/!pi*180. - 45.	; refer polarizer axis,  not work well
;dinfo.th_offset = th_offset
if dinfo.telpos eq '' then dinfo.telpos = dst.pos	; DST position 'WEST' or 'EAST'
case dinfo.wl_order of
   0: dinfo.rotang = -45. 
   1: dinfo.rotang = 45. 
endcase


save,ap,dinfo,file = savfiles.calpar
print,'demod.params: nrot= ',dinfo.nrot,',   th_offset=',dinfo.th_offset
print,'ap and new dinfo saved in ',savfiles.calpar

stop

qlmap:
;*****  QL map *****************
;--- map ---
paramfile = findfile(savfiles.calpar, count=paramok)
if paramok then restore,savfiles.calpar ;  -> ap, dinfo
nf = n_elements(rfiles.files)
sp1 = read_dstfits(rfiles.files[0],h,nslice=1)
imgsize,sp1,nx,ny
nstep=dinfo.nstep
sps = fltarr(nx,ny,nstep)
nxp = ap.ix2-ap.ix1+1
limg = fltarr(nxp,nstep)
cimg = fltarr(nxp,nstep)
j0 = nstep*dinfo.i_scan

for j=0,nstep-1 do begin
	sp1 = read_dstfits(rfiles.files[j0+j],h,nslice=1)
	limg[*,j] = sp1[ap.ix1:ap.ix2,ap.yl_l[0]]
	cimg[*,j] = sp1[ap.ix1:ap.ix2,ap.yc[0]]
endfor
tvf,congrid(cimg,200,200)
;spos = fltarr(nf)
;hs = gt_fitsheads(files)
;for i=0,nf-1 do spos[i] = fits_keyval(hs[*,i],'S4POS',/float)

stop

check:
;***** check calibration result *************************** 
restore,savfiles.drkflt	; drk[nx,ny], fltl[nxp,ny], fltr[nxp,ny]
restore,savfiles.calpar	; ap, dinfo
;UNDEFINE,pcal
imgsize,fltl,nxp,ny,nnc
;fltl= rebin(fltl,nxp,ny,1) &	fltr= rebin(fltr,nxp,ny,1)  ; <-- average all flats
if nxp gt 500 then bin = 4 
if nxp gt 300 and nxp lt 500 then bin = 2
if nxp lt 300 then bin = 1
nf=n_elements(rfiles.files)

;dinfo.j_pos = 55
j0 = dinfo.nstep * dinfo.i_scan
file1 = rfiles.files[j0+dinfo.j_pos]
print,dinfo.j_pos,'  reading ',file1
sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)
if keyword_set(dinfo.telpos) then dst.pos = dinfo.telpos

dualsp_calib1,sps,drk,fltl,fltr,ap,spsl,spsr;,/ver	; dark, two-sp, aligh, flat

;debug:
;;restore,savfiles.drkflt	; drk[nx,ny], fltl[nxp,ny], fltr[nxp,ny]
;;restore,savfiles.calpar	; ap, dinfo
;;if nxp gt 500 then bin = 4 
;;if nxp gt 300 and nxp lt 500 then bin = 2
;;if nxp lt 300 then bin = 1
;;pmax=0.01
;dinfo.th_offset = 40.
s = dualsp_demodulate(spsl,spsr,ap,dinfo,sl=sl,sr=sr,div='c') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
dispiquvr,s,bin=bin,pmax=pmax,/ialog
;;stop

debug:
;restore,savfiles.drkflt	; drk[nx,ny], fltl[nxp,ny], fltr[nxp,ny]
;restore,savfiles.calpar	; ap, dinfo
;--- correct DST polarization ---
if dst.zd eq 0. and dst.ha eq 0. then begin
	date_obs = fits_keyval(h,'DATE_OB2')
	dst = dst_st(calc_dstinfo(date_obs,dinfo.incli))
	dst.pos = dinfo.telpos
	print,'DSTinfo calculated from DATE_OB2',dst
endif

if not keyword_set(pcal) then pcal = mmdst_adjust(s[*,*,0:3], dst, dinfo.wl0, bin=bin) $
else pcal = mmdst_adjust(s[*,*,0:3], dst, dinfo.wl0, bin=bin, sconf=pcal.conf.sconf,,xpos=pcal.conf.xpos)

s2 = correct_DSTpol(s,wl0,dst,sc=sc,qin=qin,mm=mm,pars=pcal.pars)
dispiquvr,s2,bin=bin,pmax=pmax,/ialog,wid=2
;;stop

s3 = correct_Icrosstk(s2,/get_coeffs, coeffs=i2quv)
;s3 = correct_Vcrosstk(s3,/get_coeffs, coeff=v2qu)
dispiquvr,s3,bin=bin,pmax=pmax,wid=3;,/ialog ;


;path.dirs..
;cp.align parms, th_offset,nrot
;pcal = mmdst_adjust(s,wl0,telpos,dst,bin=bin,pcal=pcal)

;pcal.conf.wl0,
;          sconf.xx,yy,err,fitting params.
;pcal.pars.dstpa x4, sc, th
;pcal.pars_init.*

;save,ap,dinfo,pcal,i2quv,file=savfiles.calpar

stop
obscal:
;***** calib obs. sequence data *************************** 
restore,savfiles.drkflt	; drk[nx,ny], fltl[nxp,ny], fltr[nxp,ny]
restore,savfiles.calpar	; ap, dinfo

imgsize,fltl,nxp,ny,nn
fltl= rebin(fltl,nxp,ny,1) &	fltr= rebin(fltr,nxp,ny,1)  ; <-- average all flats
if nxp gt 500 then bin = 3 
if nxp gt 300 and nxp lt 500 then bin = 2
if nxp lt 300 then bin = 1

nf=n_elements(rfiles.files)
print,'obscal: processing '+string(dinfo.nstep,form='(i3)')+' files'
for j=0,dinfo.nstep-1 do begin
	file1=rfiles.files[dinfo.nstep*dinfo.i_scan+j]
	print,j,'  reading ',file1
	;sps = float(uint(readfits(file1,h)))c
	sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)
	;;if keyword_set(dinfo.telpos) then dst.pos = dinfo.telpos

	dualsp_calib1,sps,drk,fltl,fltr,ap,spsl,spsr
	s = dualsp_demodulate(spsl,spsr,ap,dinfo,sl=sl,sr=sr,div='c')
	;dispiquvr,s,bin=bin,pmax=pmax,/ialog,wid=0

	if dst.zd eq 0. and dst.ha eq 0. then begin
		if not keyword_set(dinfo.telpos) then throw_error,'dinfo.telpos not defined !!'
		date_obs = fits_keyval(h,'DATE_OB2')
		dst = dst_st(calc_dstinfo(date_obs,dinfo.incli))
		dst.pos = dinfo.telpos
		print,'DSTinfo calculated from DATE_OB2',dst
	endif

	if not keyword_set(pcal) then throw_error, 'pcal is not defined !!'
	s2 = correct_DSTpol(s,wl0,dst,sc=sc,qin=qin,mm=mm,pars=pcal.pars)
	;dispiquvr,s2,bin=bin,pmax=pmax,/ialog,wid=2 ;
	s3 = correct_Icrosstk(s2,/get_coeffs, coeffs=i2quv)
	dispiquvr,s3,bin=bin,pmax=pmax,/ialog,wid=3 ;
	filename_sep,file1,di,fnam,ex
	xyouts,10,10,string(j,form='(i3)')+': '+fnam,/dev,chars=3,col=0
	save,s3,ap,pcal,dinfo,h,file=path.workdir+path.outdir+'/iquv/'+fnam+'.sav'
	win2gif,path.workdir+path.outdir+'iquvimg/'+fnam+'.gif'
endfor

stop

wlident:
;***** wavelength identification *************************** 
imgsize,s,nxp,nyp,n

iprof = transpose(s[nxp/2,*,0]) 
if dinfo.wl_order eq 1  then iprof1=reverse(iprof) else iprof1=iprof
wl = wlident(iprof1,dinfo.wl0,dinfo.wl_range)
if dinfo.wl_order eq 1 then wl=reverse(wl)
save,iprof,wl,file=savfiles.wl

stop
pltprof:
;***************  plot profiles *****************
savfiles=findfile(workdir+outdir+'iquv/*.sav')
restore,savfiles[jf]	;-> sr[*,*,*]
restore,wlsav	;-> wl,iprof
iprof0 = iprof
bin=2
imgsize,sr,nxp,nyp,n5
nxp2=nxp/bin &	nyp2=nyp/bin &	ix=ipos/bin
sb = congrid(sr,nxp2,nyp2,4)
wl = rebin(wl,nyp2)
iprof = transpose(sb[ix,*,0])
qprof = transpose(sb[ix,*,1])	
uprof = transpose(sb[ix,*,2])	
vprof = transpose(sb[ix,*,3])	
if cp.wl_order then begin	; reverse wavelength
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
loadct,0
dispiquvr,sr,bin=bin,pmax=pmax,/ialog,wid=3,ipos=ipos ;

window,2,xs=nxp,ys=nyp
tvscl,sr[*,*,3]
profileski,sr[*,*,3],wy=nyp


end

