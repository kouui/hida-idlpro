; He1083_20111129.pro
;	2021.12.06	k.i.

@mmdst
@dst_pollib

@dst_file_utils
@mmdst_lib

;---------------------------------------------------------------------------
dir0 = '/mnt/HDD3TBn51/DST/sp/20220110/spec/HeI_10830/'		; root directory of raw dataset
caldir = 'calib/'					; holder for clib. data
workdir = '/nwork/kouui/data-lvl1/dstpol/20220110.giant-prominence/spec/'	; output directory in /nwork
wl0 = 10830. &	wl_range = 15. 				; central wavelength & range [A] 
wl_order = 1						; 0: top is red, 1: top is blue

;; ar1/flir20220110_085620.013.fits, DATE_OBS = '2022-01-10T08:56:16.478'
;; from slitjaw Ha the shift of S4 stopped at 08:55:04
offset_seconds = 72

;-----------------------------------------------------------------------------------
;; need to be modified while processing different scan sequence
nstep = 50						; # of step for 1 scan
telpos='WEST'						; telescope position
jf = 35 & ipos = 95 & pmax = 0.05			; spectrum #, pos on slit, max for disp

kind = 'sunspot'
search_offset = 0b
case kind of 
    'sunspot' : begin 
        darkfiles = findfile(dir0+caldir+'dark_20*.fits') ; dark
        setdir = 'ar1/' 					;    "   for obs data
        polarizer = findfile(dir0+caldir+'pol_15ms_*.fits')		; polarizer data
        avdfsav = workdir+'cal/avdforig_He.ar.sav'			; averaged dark & flat
        calparsav = workdir+'cal/calparam_He.ar.sav'		; cp structure savefile
        drkfltsav = workdir+'cal/drkflt_He.ar.sav'			; drk[nx,ny] & fltl,fltr[nxp,ny,nn]
        th_offset = 41.2	- 5.0				; WP offset in deg.
        incli = 179.79;;0.21						; inclination if dst[*,*] not available
        pmax = 0.02
        scan_head_hhmmss = [8,58,10] ; hhmmss of the head of a scan sequence
        jf = 20
        demod_div = 'c'
		ipos = 95
        UNDEFINE, sc
        nstep = 75
        UNDEFINE, vpmax
    end
    'prominence' : begin 
        darkfiles = findfile(dir0+caldir+'dark_50ms_*.fits') ; dark
        setdir = 'pr1/' 					;    "   for obs data
        polarizer = findfile(dir0+caldir+'pol_50ms_*.fits')		; polarizer data
        avdfsav = workdir+'cal/avdforig_He.pr.sav'			; averaged dark & flat
        calparsav = workdir+'cal/calparam_He.pr.sav'		; cp structure savefile
        drkfltsav = workdir+'cal/drkflt_He.pr.sav'			; drk[nx,ny] & fltl,fltr[nxp,ny,nn]
        th_offset = 36.2	 + 5.35				; WP offset in deg.
        incli = 6.65;;173.35						; inclination if dst[*,*] not available
        pmax = 10
        scan_head_hhmmss = [10,15,23]
        jf = 35
        demod_div = ''
        sc = 0
        pr_l = 335
        ipos = 125
        nstep = 50
        vpmax=5
    end
    else : throw_error, "undefined kind='"+kind+"'"
endcase

scan_head_hhmmss = offset_hhmmss_array(scan_head_hhmmss, offset_seconds)
use_scan_head = 1

file_mmdst_sfit = workdir + '/cal/mmdst_pars.20220209.sav'
use_mmdst_sfit = 1b

;-----------------------------------------------------------------------------------
;; might not need modification for sequences

outdir=setdir
flatfiles = findfile(dir0+caldir+'flat*.fits') ; flat
flatdark  = findfile(dir0+caldir+'dark_20*.fits') ; dark for flat when expo. is deferent 
hairline = findfile(dir0+caldir+'hair*.fits')		; hairline data
files = findfile(dir0+setdir+'flir*.fits')			; obs. data
wlsav = workdir+'cal/wl_He.sav'				; wavelength array file

;-----------------------------------------------------------------------------------

step = 5
case step of
	0: goto,drkflt		; average dark & flat, save in workdir
	1: goto,setparms	; manual setting of alignment parameters using hairline spectrum
	2: goto,mkflat		; make separate flats for two spectra, save in workdir
	3: goto,demoparam	; determine demodulation params. del, trhoughput ratio using pol.data
	4: goto,qlmap		; make 2D QL map for scan obs.
	5: goto,check		; check alignmenyt, flat & demodulation result
	6: goto,obscal		; calib. obs.seq., save results in workdir
	7: goto, wlident	; wave length identification
	8: goto, pltprof	; plot iquv profiles at ipos
     -1: goto, debug
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
imgsize,avflt,nxp,ny,nnc
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
if keyword_set(telpos) then cp.telpos = telpos	; DST position 'WEST' or 'EAST'
cp.rlr = rlr	; trhoughput ratio r/l
cp.del = del	; retardation of wp in deg.
cp.wl_order = wl_order	; 0: top is red, 1: top is blue  -> reft sp. is +U or -U
;;if cp.wl_order eq 1 then cp.rotang = cp.rotang + 90.
if cp.wl_order eq 1 then cp.rotang = -45. else cp.rotang = +45. ;; correct ?
cp.rotang = 45.

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
if (use_scan_head) then j0 = file_select_by_time(files, scan_head_hhmmss[0], scan_head_hhmmss[1], scan_head_hhmmss[2])

for j=0,nstep-1 do begin
	sp1 = read_dstfits(files[j0+j],h,nslice=1)
      case kind of 
        "sunspot" : limg[*,j] = sp1[cp.ix1:cp.ix2,cp.yl_l[0]]
        "prominence" : limg[*,j] = sp1[cp.ix1:cp.ix2,pr_l]
      endcase
	
	cimg[*,j] = sp1[cp.ix1:cp.ix2,cp.yc[0]]
endfor
tvf,congrid(alog10(limg),200,200)
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
if (use_scan_head) then j0 = file_select_by_time(files, scan_head_hhmmss[0], scan_head_hhmmss[1], scan_head_hhmmss[2])
cp.rotang = +45.
print, cp.rotang


file1 = files[j0+jf]
;file1=polarizer[0]
print,jf,'  reading ',file1
sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)
dualsp_calib1,sps,drk,fltl,fltr,cp,spsl,spsr;,/ver	; dark, two-sp, aligh, flat

if search_offset then begin
	n_dth = 21
	dth_offset = INDGEN(n_dth,start=-2,increment=0.5)
	res = fltarr(n_dth)
	for i=0,n_dth-1 do begin
		cp.th_offset = th_offset + dth_offset[i]
            ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
		s = dualsp_demodulate(spsl,spsr,cp,sl=sl,sr=sr,div=demod_div) 
	      res[i] = total(s[135,*,4])
	dispiquvr,s,bin=bin,pmax=pmax,coms=['I','Q','U','V','R'],/ialog
	endfor
	plot, dth_offset, res
endif else begin
	cp.th_offset = th_offset
      ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
	s = dualsp_demodulate(spsl,spsr,cp,sl=sl,sr=sr,div=demod_div)
	dispiquvr,s,bin=bin,pmax=pmax,coms=['I','Q','U','V','R'],/ialog,wid
endelse

;;s2 = correct_Icrosstk(s)
;;dispiquvr,s2,bin=bin,pmax=pmax,coms=['I','Q','U','V','R'],/ialog,wid=2

;;stop
debug:
;--- correct DST polarization ---
;; if position status "dst" unavailable, then create one base on datetime 

if dst.zd eq 0. and dst.ha eq 0. then begin
;;if 1b then begin
      ;; sunspot sample : '2022-01-10T09:00:43.450'
      ;; prominence sample : '2022-01-10T10:23:38.837'
	date_obs = fits_keyval(h,'DATE_OB2') ;;
    print, "date_obs = ", date_obs
	dst=calc_dstinfo(date_obs,incli)
	dst=dst_st(dst)
	dst.pos=telpos
	print,'DSTinfo calculated from DATE_OB2',dst
endif

;;mm = get_dstmm(wl0,telpos,dst,sc=sc,qin="slit")
if use_mmdst_sfit then begin
      restore, file_mmdst_sfit
	mm = update_mmdst(dst, pars.xn, pars.tn, pars.xc, pars.tc, pars.sc, th_vs=pars.th_vs)
endif else begin
	mm = get_dstmm(wl0,dst,sc=sc)
endelse

rmm = invert(mm)
;rmm[0,2]=-rmm[0,2]
s3 = s[*,*,0:3]
for j=0,3 do begin
	s3[*,*,j] = rmm[0,j]*s[*,*,0]
	for i=1,3 do s3[*,*,j] = s3[*,*,j] + rmm[i,j]*s[*,*,i]
endfor
;s3 = correct_Icrosstk(s3)
if kind eq 'prominence' then begin
    conf_symfit.iobs1=230 & conf_symfit.iobs2=360 & print, 'conf_symfit.iobsx modified'
    i2q = 0.0103905
    i2u = -0.00884064
    i2v = -0.000117372
    facs = [0,i2q,i2u,i2v]
    ;s3 = correct_Icrtk(s3,yr=[conf_symfit.iobs1,conf_symfit.iobs2])
    for kk=1,3 do s3[*,*,kk] = s3[*,*,kk] - s3[*,*,0] * facs[kk]
endif else s3 = correct_Icrtk(s3,yr=[conf_symfit.iobs1,conf_symfit.iobs2])

dispiquvr,s3,bin=bin,pmax=pmax,coms=['I','Q','U','V'],/ialog,wid=3 ;
;;s3 = correct_Icrosstk(s3)
;;dispiquvr,s3,bin=bin,pmax=pmax,coms=['I','Q','U','V'],/ialog,wid=4 ;

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

j0 = nstep*0
if (use_scan_head) then j0 = file_select_by_time(files, scan_head_hhmmss[0], scan_head_hhmmss[1], scan_head_hhmmss[2])
cp.rotang = +45.
print, cp.rotang

print,'obscal: processing '+string(nstep,form='(i3)')+' files'
;;sseq3 = fltarr(269,490,4,nstep)
for k=0,nstep-1 do begin
	file1=files[j0+k]
	print,k,'  reading ',file1
	;sps = float(uint(readfits(file1,h)))
	sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)

	cp.expo = fits_keyval(h,'EXP',/float)
	dualsp_calib1,sps,drk,fltl,fltr,cp,spsl,spsr;,/ver	; dark, two-sp, aligh, flat


	demod:
	;***** demodulation and cocmbine left & right sp  *****
    cp.th_offset = th_offset ;(different for sunspot and prominence)
	s = dualsp_demodulate(spsl,spsr,cp,sl=sl,sr=sr,div=demod_div);,div='i') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]

    ;; calculate dst status
	if dst.zd eq 0. and dst.ha eq 0. then begin
    ;;if 1b then begin
	      ;; sunspot sample : '2022-01-10T09:00:43.450'
	      ;; prominence sample : '2022-01-10T10:23:38.837'
		date_obs = fits_keyval(h,'DATE_OB2') ;;
	      ;print, "date_obs = ", date_obs
		dst=calc_dstinfo(date_obs,incli)
		dst=dst_st(dst)
		dst.pos=telpos
		;print,'DSTinfo calculated from DATE_OB2',dst
	endif

	;; mmdst and I-crosstalk
	if use_mmdst_sfit then begin
	      restore, file_mmdst_sfit
		mm = update_mmdst(dst, pars.xn, pars.tn, pars.xc, pars.tc, pars.sc, th_vs=pars.th_vs)
	endif else begin
		mm = get_dstmm(wl0,dst,sc=sc)
	endelse

	rmm = invert(mm)
	;rmm[0,2]=-rmm[0,2]
	s3 = s[*,*,0:3]
	for j=0,3 do begin
		s3[*,*,j] = rmm[0,j]*s[*,*,0]
		for i=1,3 do s3[*,*,j] = s3[*,*,j] + rmm[i,j]*s[*,*,i]
	endfor
	;s3 = correct_Icrosstk(s3)
      if kind eq 'prominence' then begin
	    conf_symfit.iobs1=230 & conf_symfit.iobs2=360 & print, 'conf_symfit.iobsx modified'
	    i2q = 0.0103905
	    i2u = -0.00884064
	    i2v = -0.000117372
	    facs = [0,i2q,i2u,i2v]
	    ;s3 = correct_Icrtk(s3,yr=[conf_symfit.iobs1,conf_symfit.iobs2])
	    for kk=1,3 do s3[*,*,kk] = s3[*,*,kk] - s3[*,*,0] * facs[kk]
	endif else s3 = correct_Icrtk(s3,yr=[conf_symfit.iobs1,conf_symfit.iobs2])

	filename_sep,file1,di,fnam,ex
	dispiquvr,s3,bin=bin,pmax=pmax,coms=['I','Q','U','V'],/ialog
	xyouts,10,10,string(k,form='(i3)')+': '+fnam,/dev,chars=3,col=0
	save,s3,cp,h,file=workdir+outdir+'iquv/'+fnam+'.sav'
	win2gif,workdir+outdir+'iquvimg/'+fnam+'.gif'
      ;;sseq3[*,*,*,j] = s3
      ;;stop
endfor

stop

wlident:
;***** wavelength identification *************************** 
;imgsize,s,nxp,nyp,n
;iprof = transpose(s[nxp/2,*,0])

restore,avdfsav  ; => drk[nx,ny], avflt[nx,ny,nn], hd, hf
restore,calparsav ; => cp
imgsize,avflt,nxp,ny,nnc
nxp = cp.ix2-cp.ix1+1
dualsp_align,avflt[*,*,0],cp,flt1l,flt1r
iprof = transpose(flt1l[nxp/2,*,0])
UNDEFINE, drk
UNDEFINE, avflt
;UNDEFINE, cp
UNDEFINE, flt1l
UNDEFINE, flt1r

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
;sb = congrid(s2,nxp2,nyp2,5)
sb = congrid(s3,nxp2,nyp2,4)

if kind eq 'prominence' then begin
	qprof0 = transpose(sb[ix,*,1])	
	uprof0 = transpose(sb[ix,*,2])	
	vprof0 = transpose(sb[ix,*,3])	
	if cp.wl_order then begin	; reverse wavelength
		qprof0=reverse(qprof0)
		uprof0=reverse(uprof0)
		vprof0=reverse(vprof0)
	endif
endif

if kind eq 'prominence' and 0b then begin
      comment = ['I->Q','I->U','I->V']
	for i=1,3 do begin
	  	if i eq 3 then begin 
		  	xr1 = 300 & xr2 = 320
		endif else begin
	  		xr1 = 112 & xr2 = 118
		endelse
		case i of
			1 : fac=1.4;1.05;1.18
			2 : fac=0.6;1.05;6.0
			3 : fac=1.0;1.05
		endcase
		ifac = 115
		fac *= mean(mean(sb[ifac-5:ifac+4,xr1:xr2,i],dimension=2)/mean(sb[ix-5:ix+4,xr1:xr2,0],dimension=2))
             print, '[I correction factor] ' + comment[i-1] + ' = ', fac
	  	for j=0,nxp2-1 do begin
		  	;;fac = mean(sb[j,xr1:xr2,i],dimension=2)/mean(sb[j,xr1:xr2,0],dimension=2)
			sb[j,*,i] = sb[j,*,i] - sb[j,*,0] * fac
	  	endfor
	endfor
	;;sb[*,*,i] = sb[*,*,i] - sb[*,*,0] * reform(mean(sb[*,xr1:xr2,i],dimension=2)/mean(sb[*,xr1:xr2,0],dimension=2),nxp2,1,1)
endif

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
case kind of
    "prominence" : yr = 0.04*[-1,1]
     "sunspot"   : yr = 0.1*[-1,1]
endcase
if kind eq 'prominence' then begin
	fac = 1/max(iprof)
	qprof *= fac
	uprof *= fac
	vprof *= fac
	qprof0 *= fac
	uprof0 *= fac
	vprof0 *= fac
endif
;;yr=0.1*[-1,1]
wln=wl/10
xr=[wln[0],wln[nyp2-1]]
cs=2.2
color0 = 'ffc466'x
title=fnam+'  x='+string(ipos,form='(i3)')
plot,wln,iprof,pos=box+yoff*(0.1+wy1*3),/norm,xtickname=blank,chars=cs,xr=xr,xstyle=1,yr=[0,max(iprof)],ystyle=1,title=title
xyouts,wln[20],1.,'I',/data,chars=2.5
plot,wln,qprof,pos=box+yoff*(0.1+wy1*2),/norm,xtickname=blank,/noerase,chars=cs,xr=xr,xstyle=1,yr=yr,ystyle=1
oplot,xr,[0,0],line=1
;oplot,wln,qprof0,line=2,color=color0
xyouts,wln[20],yr[1]*0.6,'Q/Imax',/data,chars=2.5
plot,wln,uprof,pos=box+yoff*(0.1+wy1*1),/norm,xtickname=blank,/noerase,chars=cs,xr=xr,xstyle=1,yr=yr,ystyle=1
oplot,xr,[0,0],line=1
;oplot,wln,uprof0,line=2,color=color0
xyouts,wln[20],yr[1]*0.6,'U/Imax',/data,chars=2.5
plot,wln,vprof,pos=box+yoff*(0.1+wy1*0),/norm,/noerase,xtitle='wavelength [nm]',chars=cs,xr=xr,xstyle=1,yr=yr,ystyle=1
oplot,xr,[0,0],line=1
;oplot,wln,vprof0,line=2,color=color0
xyouts,wln[20],yr[1]*0.6,'V/Imax',/data,chars=2.5

;win2gif,workdir+outdir+'profs.gif'

window,2,xs=nxp*4,ys=nyp
loadct,0
for i=0,3 do begin
	if i eq 0 then tvscl,sb[*,*,i],nxp*i,0 else tvscl,clip(sb[*,*,i],pmax,-pmax),nxp*i,0
	;;tv,sb[*,*,i],nxp*i,0
	;tvscl,sb[*,*,i],nxp*i,0
	
	draw,ix*[1,1]+nxp*i,[0,nyp]
endfor

stop


end

