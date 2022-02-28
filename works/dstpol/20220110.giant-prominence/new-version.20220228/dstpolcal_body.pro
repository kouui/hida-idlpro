; dstpolcal_body.pro
;	2022.02.15	k.i.	from template
;	2022.02.23	k.i.	dinfo.camera

dir0 = path.rootdir+path.caldatdir
undefine,rfiles
rfiles = create_struct( $
	'dark', findfile(dir0+path.darkfile), $			; dark
	'flat', findfile(dir0+path.flatfile), $			; flat
	'flatdark', '',	 $					; dark for flat
	'hairline', findfile(dir0+path.hairline), $ 		; hairline
	'polarizer', findfile(dir0+path.polarizer), $		; polarizer
	'files', findfile(path.rootdir+path.obsdir+'*.fits') $	; obs. data
	)
if path.flatdark ne '' then rfiles.flatdark = findfile(dir0+path.flatdark)

calpardir = path.workdir+path.calpardir
undefine,savfiles
savfiles = {save_files, $
	drkflt1: calpardir+'drkflt1_'+path.calid+'.sav', $	; averaged dark & flat
	drkflt2: calpardir+'drkflt2_'+path.calid+'.sav', $	; drk[nx,ny] & fltl,fltr[nxp,ny,nn]
	ap: calpardir+'alignparm_'+path.calid+'.sav', $		; align params
	dinfo: calpardir+'dinfo_'+path.calid+'.sav', $		; data info.
	pcal: calpardir+'pcal_'+path.calid+'.sav', $		; DST pol. consts for Mueller matrix
	wl: calpardir+'wl_'+path.calid+'.sav' $ 		; wl[*] save file
	}

help,rfiles
help,dinfo
;---------------------------------------------------------------------------

menu = ['0: average drkflt', $		; average dark & flat, save in workdir
	'1: set align params', $ 	; manual setting of alignment parameters using hairline spectrum
	'2: make separate flats', $	; make separate flats for two spectra, save in workdir
	'3: qlmap', $			; make 2D QL map for scan obs.
	'4: demodulation params', $	; determine demodulation params. del, trhoughput ratio using pol.data
	'5: check cal. result', $	; check alignmenyt, flat & demodulation result
	'6: obsdata cal', $		; calib. obs.seq., save results in workdir
	'7: wl-ident', $		; wave length identification
	'8: plot profs', $		; plot iquv profiles at ipos
	'9: IQUV map', $		; make IQUV map
	'10: cancel', $
	'11: debug' $
	]

step = smenu(menu,xpos=500,ypos=200,title='DST polcal')
case step of
	0: goto,drkflt		; average dark & flat, save in workdir
	1: goto,setparms	; manual setting of alignment parameters using hairline spectrum
	2: goto,mkflat		; make separate flats for two spectra, save in workdir
	3: goto,qlmap		; make 2D QL map for scan obs.
	4: goto,demoparam	; determine demodulation params. del, trhoughput ratio using pol.data
	5: goto,check		; check alignment, flat & demodulation result
	6: goto,obscal		; calib. obs.seq., save results in workdir
	7: goto,wlident		; wave length identification
	8: goto,pltprof		; plot iquv profiles at ipos
	9: goto,iquvmap		; show IQUV map and plot
	10: stop
	11: goto,debug
endcase



drkflt: 
;***** make dark & flat average *******************************
print,'making dark & flat average ....'
if not keyword_set(rfiles.dark) then begin & print,'dark not defined' & stop & end
if not keyword_set(rfiles.flat) then begin & print,'flat not defined' & stop & end
calpardir = path.workdir+path.calpardir
if not file_test(calpardir) then file_mkdir,calpardir
mk_drkflt, rfiles.dark,rfiles.flat,drk,avflt, flatdark=rfiles.flatdark, hd=hd,hf=hf ,savfile=savfiles.drkflt1

stop

setparms:
;*****  set params for dual-beam alignment   ***************
print,'read ',savfiles.drkflt1
restore,savfiles.drkflt1  ; => drk[nx,ny], avflt[nx,ny,nn], hd, hf
hlsp = read_dstfits(rfiles.hairline[0],hh,cam=cam,dst=dst,tim=tim)
imgsize,hlsp,nx,ny,nn
hlsp=rebin(hlsp,nx,ny,1)-drk

dualsp_align,hlsp,ap,spl,spr,/measure	; 
ap.filename = rfiles.hairline[0]

if not file_test(calpardir) then file_mkdir,calpardir
save,ap,file = savfiles.ap
print,'new align params* saved in ',savfiles.ap

stop

mkflat:
;*****  make separate flat  ******
restore,savfiles.drkflt1  ; => drk[nx,ny], avflt[nx,ny,nn], hd, hf
restore,savfiles.ap ; => ap
imgsize,avflt,nx,ny,nn
avflt1 = rebin(avflt,nx,ny,1)
nxp = ap.ix2-ap.ix1+1
print,'Making fltl[*,*], fltr[*,*]'
;fltl = fltarr(nxp,ny,nn)  &	fltr = fltl
;for i=0,nn-1 do begin
;	print,i
;	dualsp_align,avflt[*,*,i],ap,flt1l,flt1r
;	flt1l = flt1l/rebin(rebin(flt1l,1,ny),nxp,ny)
;	flt1r = flt1r/rebin(rebin(flt1r,1,ny),nxp,ny)
;	fltl[*,*,i] = flt1l
;	fltr[*,*,i] = flt1r
;endfor

dualsp_align,avflt1,ap,avfltl,avfltr
;flt1l = rebin(fltl,nxp,ny,1)
;flt1r = rebin(fltr,nxp,ny,1)
;save,drk,fltl,fltr,flt1l,flt1r,file = savfiles.drkflt
;fltl = flt1l
;fltr = flt1r

fltl = avfltl/rebin(rebin(avfltl,1,ny),nxp,ny)
fltr = avfltr/rebin(rebin(avfltr,1,ny),nxp,ny)

dualsp_calib1,avflt1,drk,fltl,fltr,ap,fltspl,fltspr
window,2
tvf,[fltl,fltr]

if not file_test(calpardir) then file_mkdir,calpardir
save,drk,fltl,fltr,avfltl,avfltr,fltspl,fltspr,file = savfiles.drkflt2
print,'dark and separate flats saved in  '+ savfiles.drkflt2

stop

qlmap:
;*****  QL map *****************
;--- map ---
restore,savfiles.ap	; -> ap
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
	print,j,'  ',rfiles.files[j0+j]
	sp1 = read_dstfits(rfiles.files[j0+j],h,nslice=1)
	limg[*,j] = sp1[ap.ix1:ap.ix2,ap.yl_l[0]]
	cimg[*,j] = sp1[ap.ix1:ap.ix2,ap.yc[0]]
endfor
tvf,congrid(cimg,200,200)
;spos = fltarr(nf)
;hs = gt_fitsheads(files)
;for i=0,nf-1 do spos[i] = fits_keyval(hs[*,i],'S4POS',/float)
wshow

stop

demoparam:
;*****  demod. param. *****************************
restore,savfiles.ap
restore,savfiles.drkflt2	; -> drk,fltl,fltr,avfltl,avfltr,fltspl,fltspr

spp = read_dstfits(rfiles.polarizer[0],hp,cam=cam,dst=dst,tim=tim)
imgsize,spp,nx,ny,nn
rotp = fits_keyval(hp,'ROTP',/fix)
expo = fits_keyval(hp,'EXP',/float)
dinfo.expo = expo
dinfo.rotp = rotp
dinfo.bin = fits_keyval(hp,'BIN',/fix)

dinfo.camera = fits_keyval(hp,'CAMERA',/compress)
mprof1 =  reform(rebin(spp[ap.ix1:ap.ix2,ap.yc[0]:ap.yc[1],*],1,1,nn),nn)
mprof2 =  reform(rebin(spp[ap.ix1+ap.ddx:ap.ix2+ap.ddx, $
			   ap.yc[0]+ap.ddy:ap.yc[1]+ap.ddy,*],1,1,nn),nn)
th = findgen(nn)/nn * dinfo.expo * nn /dinfo.rotp * 2*!pi	; position of WP, [rad]
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
dinfo.rlr = rlr			; trhoughput ratio r/l
;dinfo.nrot = expo*nn/rotp	; # of waveplate rotation in a set
k = (k1+k2)/2.
dinfo.nrot = expo*nn/rotp*k/4.	; # of waveplate rotation in a set


;th_offset_ = atan(ss,cc)/!pi*180. - 45.	; refer polarizer axis,  not work well
;dinfo.th_offset = th_offset
if dinfo.telpos eq '' then dinfo.telpos = dst.pos	; DST position 'WEST' or 'EAST'
case dinfo.wl_order of
   0: dinfo.rotang = -45. 
   1: dinfo.rotang = 45. 
endcase

;----  determine th_offset ---
print,'eliminate R by adjusting th_offset..'
j0 = dinfo.nstep*dinfo.i_scan
file1 = rfiles.files[j0+dinfo.j_pos]
;;file1 = rfiles.polarizer[0]
print,dinfo.j_pos,'  reading ',file1
sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)

dualsp_calib1,sps,drk,fltl,fltr,ap,spsl,spsr;,/ver	; dark, two-sp, aligh, flat

debug:
t0 = systime(/second)
s = dualsp_demodulate(spsl,spsr,ap,dinfo,sl=sl,sr=sr,div='c') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
t1 = systime(/second)
print,'demodulation took ',t1-t0, 'sec'
bin = binfact(s)
dispiquvr,s,bin=bin,pmax=pmax,/ialog
com = 'th_offset = '+string(dinfo.th_offset,form='(f7.2)')+'   ==> '
ans = wdgetstr(com,xpos=500,ypos=300,title='eliminate R')
while ans ne '' do begin
	dinfo.th_offset = float(ans)
	s = dualsp_demodulate(spsl,spsr,ap,dinfo,sl=sl,sr=sr,div='c') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
	dispiquvr,s,bin=bin,pmax=pmax,/ialog
	com = 'th_offset = '+string(dinfo.th_offset,form='(f7.2)')+'   ==> '
	ans = wdgetstr(com,xpos=500,ypos=300,title='eliminate R')
endwhile


help,dinfo,/st
if not file_test(calpardir) then file_mkdir,calpardir
save,dinfo,file = savfiles.dinfo
print,'demod.params: nrot= ',dinfo.nrot,',   th_offset=',dinfo.th_offset
print,'dinfo saved in ',savfiles.dinfo

stop


check:
;***** check calibration result *************************** 
print,'>> check pol. calibration <<'
restore,savfiles.drkflt2	; drk[nx,ny], fltl[nxp,ny], fltr[nxp,ny]
restore,savfiles.ap	; ap
imgsize,fltl,nxp,ny,nn
bin = binfact(fltl)
nf=n_elements(rfiles.files)


;--- correct DST polarization ---
if dst.zd eq 0. and dst.ha eq 0. then begin
	date_obs = fits_keyval(h,'DATE_OB2')
	dst = dst_st(calc_dstinfo(date_obs,incli))
	dst.pos = dinfo.telpos
	print,'DSTinfo calculated from DATE_OB2',dst
endif

if keyword_set(dinfo.telpos) then dst.pos = dinfo.telpos
pcal = mmdst_adjust(s[*,*,0:3], dst, dinfo.wl0, bin=bin)
s2 = correct_DSTpol(s, dinfo.wl0, dst, sc=pcal.pars.sc, mm=mm, pars=pcal.pars)
s3 = correct_Icrosstk(s2,/get_coeffs, coeffs=i2quv)
bin = binfact(s)
dispiquvr,s3,bin=bin,pmax=pmax,wid=2;,/ialog

save,pcal,i2quv,file=savfiles.pcal

sr = s3
filename_sep,file1,di,fnam,ex
outdir = path.workdir+path.outdir
if not file_test(outdir) then file_mkdir,outdir
save,sr,file=outdir+fnam+'.sav'
print,'result sr[*,*,4] saved in ',outdir

stop
obscal:
;***** calib obs. sequence data *************************** 
print,'>> calibrate obs. data sequence <<'
restore,savfiles.drkflt2 ; drk[nx,ny], fltl & fltr[nxp,ny], avfltl,avfltr[nxp,ny], fltspl,fltspr[nxp,ny]
restore,savfiles.ap	; ap
restore,savfiles.pcal	; pcal
imgsize,fltl,nxp,ny,nn
bin = binfact(fltl)

files = rfiles.files
nf=n_elements(files)
nstep = dinfo.nstep
print,'obscal: processing '+string(nstep,form='(i3)')+' files'
outdir = path.workdir+path.outdir
if not file_test(outdir) then file_mkdir,outdir

for j=0,nstep-1 do begin
	file1=files[dinfo.i_scan+j]
	print,j,'  reading ',file1
	sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)
	if dst.zd eq 0. and dst.ha eq 0. then begin
		date_obs = fits_keyval(h,'DATE_OB2')
		dst = dst_st(calc_dstinfo(date_obs,incli))
		dst.pos = dinfo.telpos
		print,'DSTinfo calculated from DATE_OB2',dst
	endif

	dualsp_calib1,sps,drk,fltl,fltr,ap,spsl,spsr;,/ver	; dark, two-sp, aligh, flat
	s = dualsp_demodulate(spsl,spsr,ap,dinfo,sl=sl,sr=sr,div='c') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
	dispiquvr,s,bin=bin,pmax=pmax,/ialog,wid=0
	s2 = correct_DSTpol(s,dinfo.wl0,dst,sc=sc,mm=mm,pars=pcal.pars)
	s3 = correct_Icrosstk(s2, coeffs=i2quv, /get_coeffs)
	sr = s3
	;sr = correct_Vcrosstk(s3, coeffs=v2qu)
	dispiquvr,sr,bin=bin,pmax=pmax,/ialog,wid=3 ;
	filename_sep,file1,di,fnam,ex
	xyouts,10,10,string(j,form='(i3)')+': '+fnam,/dev,chars=3,col=0
	save,sr,pcal,h,file=outdir+fnam+'.sav'
	;win2gif,workdir+outdir+'iquvimg/'+fnam+'.gif'
endfor

stop

wlident:
;***** wavelength identification *************************** 
print,'>> identify wavelength <<'
print,'restoring ',savfiles.drkflt2
restore,savfiles.drkflt2	; drk[nx,ny], fltl & fltr[nxp,ny], avfltl & avfltr[nxp,ny],fltspl & fltspr[nxp,ny]
imgsize,fltspl,nxp,nyp,n
iprof = transpose(fltspl[nxp/2,*,0]) 
if dinfo.wl_order eq 1  then iprof1=reverse(iprof) else iprof1=iprof
wshow
wl = wlident(iprof1, dinfo.wl0, dinfo.wl_range)
if dinfo.wl_order eq 1 then wl=reverse(wl)

save,iprof,wl,file=savfiles.wl
print,'wl[*] saved in ',savfiles.wl

stop

pltprof:
;***************  plot profiles *****************
print,'>> plot Stokes profiles <<'
diri = path.workdir+path.outdir
savfile1 = dialog_pickfile(path=diri,filter='*.sav')
restore,savfile1	;-> sr[*,*,4]
restore,savfiles.wl	;-> wl,iprof
iprof0 = iprof
imgsize,sr,nxp,nyp,nn
bin = binfact(sr)
filename_sep,savfile1,di,fnam,ex

dispiquvr,sr,bin=bin,pmax=pmax,wid=0;,/ialog
nx2 = nxp/bin &	ny2=nyp/bin
dd = (!d.x_size-nx2*4)/5

window,2,xs=1000,ys=700
!err=0
while !err ne 4 do begin	; 4=right button
	wset,0
	loadct,0
	cursor,px,py,3,/dev
	if !err eq 4 then breakc
	ix1 = (px-dd) mod (nx2+dd)
	for j=0,3 do plots,(dd+ix1)*[1,1]+(dd+nx2)*j,[0,ny2],line=1,/dev
	wset,2
	wshow,2
	stretch,255,0
	plotiquv,sr,wl=wl,title=fnam,ix=ix1*bin,worder=dinfo.wl_order,pmax=pmax;,bin=binp
endwhile

iquvmap:
;***************  show iquvmap *****************
files = findfile(path.workdir+path.outdir+'*.sav')
nf = n_elements(files)
restore,files[dinfo.nstep/2]	; -> sr[*,*,4],pacal
restore,savfiles.wl	; wl[]
imgsize,sr,nx,ny,n4
smap = fltarr(nx,dinfo.nstep,4)
bin = binfact(sr)
nx2 = nx/bin &	ny2 = ny/bin
dispiquvr,sr,bin=bin,pmax=pmax,/ialog,wid=0,dd=dd
cursor,px,py,3,/dev
ix1 = (px-dd) mod (nx2+dd) &	j1 = (py-dd)
for i=0,3 do begin
	x0 = dd+i*(nx2+dd)
	plots,x0+[0,nx2],dd+j1+[0,0],line=1,/dev
endfor

for i=0,dinfo.nstep-1 do begin
	print,i,'  ',files[i]
	restore,files[i]
	smap[*,i,*] = sr[*,j1*bin,*]
endfor
nx3 = 300 &	ny3 = 200
smap3 = congrid(smap,nx3,ny3,n4)
dispiquvr,smap3,bin=1,pmax=pmax,wid=4

window,3,xs=1000,ys=700
!err=0
while !err ne 4 do begin	; 4=right button
	wset,4
	wshow,4
	loadct,0
	cursor,px,py,3,/dev
	if !err eq 4 then break
	ix1 = (px-dd) mod (nx3+dd) &	j1 = (py-dd)
	;dispmaps,maps,coms,yfact=yfact,pmax=pmax,dd=dd,bin=bin
	dispiquvr,smap3,bin=1,pmax=pmax,wid=4
	plots,dd+ix1+indgen(n4)*(nx3+dd),py*replicate(1,n4),psym=2,/dev
	j = float(j1)*dinfo.nstep/ny3
	ipos = float(ix1)*nx/nx3
	file1 = files(j)
	print,file1
	restore,file1
	;dispiquvr,sr,bin=bin,pmax=pmax,/ialog,wid=3,ipos=ipos
	wset,3
	stretch,255,0
	filename_sep,file1,di,fnam,ex
	plotiquv,sr,wl=wl,title=fnam,ix=ipos,worder=dinfo.wl_order,pmax=pmax,bin=1
endwhile








