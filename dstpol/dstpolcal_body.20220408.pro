; dstpolcal_body.pro
;	2022.02.15	k.i.	from template
;	2022.02.23	k.i.	dinfo.camera
;	2022.03.01	k.i.	mkspflat, correct_QUVconti()
;	2022.03.13	k.i.	rep_badframe, chk_1st_frame, xalign_sps, gif save
;	2022.03.19	k.i.	dinfo.xalign, com
;	2022.04.07	k.i.,u.k.,	cal structure

dir0 = path.rootdir+path.caldatdir
undefine,rfiles
rfiles = create_struct( $
	'dark', findfile(dir0+path.darkfile), $			; dark
	'flat', findfile(dir0+path.flatfile), $			; flat
	'flatdark', '',	 $					; dark for flat
	'hairline', findfile(dir0+path.hairline), $ 		; hairline
	'polarizer', findfile(dir0+path.polarizer), $		; polarizer
	'files', findfile(path.rootdir+path.obsdir+path.obsdat) $	; obs. data
	)
if path.flatdark ne '' then begin
	flatdarks = findfile(dir0+path.flatdark)
	rfiles.flatdark = flatdarks[0]
endif

calpardir = path.workdir+path.calpardir
cal.flat_org = calpardir+'flat_org.sav'

;help,rfiles
;help,dinfo
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

;step = smenu(menu,xpos=500,ypos=200,title='DST polcal')
step = 5
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
mk_drkflt, rfiles.dark,rfiles.flat,drk,avflt, flatdark=rfiles.flatdark, hd=hd,hf=hf ,savfile=cal

stop

setparms:
;*****  set params for dual-beam alignment   ***************
print,'restoring ',cal.dark
restore,cal.dark  ; => drk[nx,ny]
print,'reading ',rfiles.hairline[0]
hlsp = read_dstfits(rfiles.hairline[0],hh,cam=cam,dst=dst,tim=tim)
imgsize,hlsp,nx,ny,nn
hlsp=rebin(hlsp,nx,ny,1)-drk

dualsp_align,hlsp,ap,spl,spr,/measure	; 
ap.filename = rfiles.hairline[0]

if not file_test(calpardir) then file_mkdir,calpardir
save,ap,file = cal.ap
print,'new align params* saved in ',cal.ap

stop

mkflat:
;*****  make separate flat  ******
print,'restoring ',cal.flat_org
restore,cal.flat_org  ; => avflt[nx,ny,nn], hf
restore,cal.ap ; => ap
imgsize,avflt,nx,ny,nn
avflt1 = rebin(avflt,nx,ny,1)
nxp = ap.ix2-ap.ix1+1
print,'Making fltl[*,*], fltr[*,*]'

dualsp_align,avflt1,ap,avfltl,avfltr
x00 = ap.yl_l[0]
fltl = mkspflat(avfltl, x00, xc=ap.yc, xw=30, xf=7, delx=delxl, /vert)
fltr = mkspflat(avfltr, x00, xc=ap.yc, xw=30, xf=7, delx=delxl, /vert)

dualsp_calib1,avflt1,drk,fltl,fltr,ap,fltspl,fltspr
window,2
tvf,[fltl,fltr]

if not file_test(calpardir) then file_mkdir,calpardir
save,fltl,fltr,avfltl,avfltr,fltspl,fltspr,file = cal.flat
print,'dark and separate flats saved in  '+ cal.flat

stop

qlmap:
;*****  QL map *****************
;--- map ---
print,'make QL map,  restoring ',cal.ap
restore,cal.ap	; -> ap
nf = n_elements(rfiles.files)
sp1 = read_dstfits(rfiles.files[0],h,nslice=1)
imgsize,sp1,nx,ny

window,2,xs=nx,ys=ny
tvscl,sp1
print,'click spectral position on left sp'
cursor,px,py,3,/dev

if dinfo.nstep eq -1 then nstep = nf else nstep = dinfo.nstep
sps = fltarr(nx,ny,nstep)
nxp = ap.ix2-ap.ix1+1
limg = fltarr(nxp,nstep)
j0 = nstep*dinfo.i_scan

for j=0,nstep-1 do begin
	print,j,'  ',rfiles.files[j0+j]
	sp1 = read_dstfits(rfiles.files[j0+j],h,nslice=1)
	limg[*,j] = sp1[ap.ix1:ap.ix2,py]
endfor
tvf,congrid(limg,200,200)
wshow

stop

demoparam:
;*****  demod. param. *****************************
print,'set demodulation params..'
restore,cal.ap
restore,cal.dark	; -> drk
restore,cal.flat	; -> fltl,fltr,avfltl,avfltr,fltspl,fltspr

print,'reading pol.data ',rfiles.polarizer[0]
spp = read_dstfits(rfiles.polarizer[0],hp,cam=cam,dst=dst,tim=tim)
imgsize,spp,nx,ny,nn
rotp = fits_keyval(hp,'ROTP',/fix)
expo = fits_keyval(hp,'EXP',/float)
dinfo.expo = expo
dinfo.rotp = rotp
dinfo.bin = fits_keyval(hp,'BIN',/fix)
for i=0,nn-1 do spp[*,*,i] = spp[*,*,i]-drk

dinfo.camera = fits_keyval(hp,'CAMERA',/compress)
mprof1 =  reform(rebin(spp[ap.ix1:ap.ix2,ap.yc[0]:ap.yc[1],*],1,1,nn),nn)
mprof2 =  reform(rebin(spp[ap.ix1+ap.ddx:ap.ix2+ap.ddx, $
			   ap.yc[0]+ap.ddy:ap.yc[1]+ap.ddy,*],1,1,nn),nn)
window,2,xs=1550,ys=500
plot,mprof2,title='modullation w/ polarizer',chars=2
oplot,mprof1,line=2,col=250

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
   1: dinfo.rotang = +45. 
endcase
;dinfo.rotang = 0.

;----  determine th_offset ---
print,'eliminate R by adjusting th_offset..'
j0 = dinfo.nstep*dinfo.i_scan
file1 = rfiles.files[j0+dinfo.j_pos]
if check_pol then file1 = rfiles.polarizer[0]

print,dinfo.j_pos,'  reading ',file1
sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)

sps = rep_missing_pix(sps,miss=miss) 		; replace missing pixels with average i-1 & i+1 frame
if miss ne 0 then dinfo.com=dinfo.com+'/missing_pix '
dualsp_calib1,sps,drk,fltl,fltr,ap,spsl,spsr;,/ver	; dark, two-sp, aligh, flat
chk_1st_frame,spsl,spsr,ap,dinfo,xprof=xprof	; replace 1st frame by 1rot later if its level is unrelistic
						; xprof[nxp,nn]  -  continuum intensity along slit & modulation
if dinfo.xalign then begin
	xalign_sps,spsl,spsr,xprof,dx=dx		;  align sps[*,*,nn] in slit direction
	dinfo.com = '/xalign '
endif

debug:
t0 = systime(/second)
s0 = dualsp_demodulate(spsl,spsr,ap,dinfo,sl=sl,sr=sr,div='c') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
t1 = systime(/second)
print,'demodulation took ',t1-t0, 'sec'

if file_test(cal.pcal) then begin
	restore,cal.pcal
	pars=pcal.pars
	sc=pars.sc
	print,'pcal restored from ',cal.pcal
	com='[from Zemman]'
endif else begin
	pars=0
	sc=0
	;if not check_pol then s0 = srot(s0, dinfo.rotang)
	print,'pars from Anan calibration'
	com='[from Anan]'
endelse

if dst.zd eq 0. and dst.ha eq 0. then begin
	date_obs = fits_keyval(h,'DATE_OB2')
	dst = dst_st(calc_dstinfo(date_obs,dinfo.incli))
	dst.pos = dinfo.telpos
	print,'DSTinfo calculated from DATE_OB2',dst
endif

;s0 = srot(s0, dinfo.rotang)

s1=s0
if not check_pol then s1[*,*,0:3] = correct_DSTpol(s0, dinfo.wl0, dst, sc=sc, mm=mm, pars=pars)
;case drot45 of
;	1 : begin
;	phi2 = 2 * (+1) * dinfo.rotang/180*!pi
;	cos2 = cos(phi2) &	sin2 = sin(phi2)
;	s01 = s1
;	s1[*,*,1] = s01[*,*,1] * cos2 + s01[*,*,2] * sin2
;	s1[*,*,2] = -1 * s01[*,*,1] * sin2 + s01[*,*,2] * cos2
;	end
;	else:
;endcase
bin = binfact(s0)
if check_pol then title = 'demo only' else title = 'demo & mmdst '+com
dispiquvr,s1,bin=bin,pmax=pmax,/ialog,title=title
means = rebin(s1,1,1,5) &	print,reform(means,5)/means[0]
com = 'th_offset = '+string(dinfo.th_offset,form='(f7.2)')+'   ==> '
ans = wdgetstr(com,xpos=500,ypos=300,title='eliminate R')
while ans ne '' do begin
	dinfo.th_offset = float(ans)
	s0 = dualsp_demodulate(spsl,spsr,ap,dinfo,sl=sl,sr=sr,div='c') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
	if not check_pol then s1[*,*,0:3] = correct_DSTpol(s0, dinfo.wl0, dst, sc=sc, mm=mm, pars=pars)
	dispiquvr,s1,bin=bin,pmax=pmax,/ialog,title='demo & mmdst '+com
	means = rebin(s1,1,1,5) &	print,reform(means,5)/means[0]
	com = 'th_offset = '+string(dinfo.th_offset,form='(f7.2)')+'   ==> '
	ans = wdgetstr(com,xpos=500,ypos=300,title='eliminate R')
endwhile


help,dinfo,/st
if not file_test(calpardir) then file_mkdir,calpardir
save,dinfo,file = cal.dinfo
print,'demod.params: nrot= ',dinfo.nrot,',   th_offset=',dinfo.th_offset
print,'dinfo saved in ',cal.dinfo

stop


check:
;***** check calibration result *************************** 
print,'>> check pol. calibration <<'
restore,cal.dark	; drk[nx,ny]
restore,cal.flat	; fltl[nxp,ny], fltr[nxp,ny]
restore,cal.ap	; ap
if file_test(cal.pcal) then restore,cal.pcal	; pcal
imgsize,fltl,nxp,ny,nn
bin = binfact(fltl)
nf = n_elements(rfiles.files)

;--- correct DST polarization ---
if dst.zd eq 0. and dst.ha eq 0. then begin
	date_obs = fits_keyval(h,'DATE_OB2')
	dst = dst_st(calc_dstinfo(date_obs,dinfo.incli))
	dst.pos = dinfo.telpos
	print,'DSTinfo calculated from DATE_OB2',dst
endif
com = 'demo & mmdst'
if keyword_set(dinfo.telpos) then dst.pos = dinfo.telpos
if dinfo.adj_dstpol then begin
	pcal = mmdst_adjust(s0[*,*,0:3], dst, dinfo.wl0, bin=bin) 
	com=com+'[from Zeeman]'
endif else begin
	if not file_test(cal.pcal) then begin
		pcal = mmdst_adjust(s0[*,*,0:3], dst, dinfo.wl0, bin=bin, /anan) 
		com=com+'[from Anan]'
	endif else begin 
		;pcal.pars.tn += !pi
		;pcal.pars.tc += !pi
		com=com+'[from Zeeman]'
	endelse
endelse
help, pcal.pars
print, "incli [deg] = ", dinfo.incli, dst.incli
s1 = correct_DSTpol(s0, dinfo.wl0, dst, sc=pcal.pars.sc, mm=mm, pars=pcal.pars)

print, "incli [deg] = ", dinfo.incli, dst.incli

case dinfo.correct_I2quv of
	-1: begin
		s2 = s1
		for iii=0,2 do s2[*,*,iii+1] -= s2[*,*,0] * my_i2quv[iii]
		com = com + ' & I2quv [from my_i2quv]'
	end
   1: begin
	s2 = correct_Icrosstk(s1, coeffs=pcal.i2quv) 
	com = com + ' & I2quv [from pcal]'
	end
   2: begin
	s2 = correct_Icrosstk(s1,/get_coeffs, coeffs=i2quv) 
	pcal.i2quv = i2quv
	print,i2quv
	com = com + ' & I2quv [from data]'
	end
   else: begin
	s2 = s1
	end
endcase

s = correct_QUVconti(s2,ap.yc)
com = com + ' & rm QUVconti.'
bin = binfact(s)
dispiquvr,s,bin=bin,pmax=pmax,wid=2,title=com;,/ialog

if dinfo.adj_dstpol then begin
	save,pcal,file=cal.pcal
	print,'pcal saved in ',cal.pcal
endif

filename_sep,file1,di,fnam,ex
outdir = path.workdir+path.outdir
if not file_test(outdir) then file_mkdir,outdir
save,s,file=outdir+fnam+'.sav'
print,'result s[*,*,4] saved in ',outdir

stop

obscal:
;***** calib obs. sequence data *************************** 
print,'>> calibrate obs. data sequence <<'
restore,cal.dark	; drk[nx,ny]
restore,cal.flat	; fltl[nxp,ny], fltr[nxp,ny]
restore,cal.ap		; ap
restore,cal.pcal	; pcal
imgsize,fltl,nxp,ny,nn
bin = binfact(fltl)

files = rfiles.files
nf = n_elements(files)
if dinfo.nstep eq -1 then nstep = nf else nstep = dinfo.nstep

print,'obscal: processing '+string(nstep,form='(i3)')+' files'
outdir = path.workdir+path.outdir
if not file_test(outdir) then file_mkdir,outdir
gifdir = outdir+'iquv/'
if not file_test(gifdir) then file_mkdir,gifdir


for j=0,nstep-1 do begin
	dinfo.com = ''
	file1=files[dinfo.i_scan*dinfo.nstep+j]
	com1 = strcompress(string(j)+'/'+string(nstep),/remove_all)
	print,com1+'  reading ',file1
	sps = read_dstfits(file1,h,cam=cam,dst=dst,tim=tim)
	if dst.zd eq 0. and dst.ha eq 0. then begin
		date_obs = fits_keyval(h,'DATE_OB2')
		dst = dst_st(calc_dstinfo(date_obs,dinfo.incli))
		dst.pos = dinfo.telpos
		print,'DSTinfo calculated from DATE_OB2,  ZD=', string(dst.zd,form='(f6.4)'), $
			'  HA=',string(dst.ha,form='(f6.4)'),'  Azim=',string(dst.az,form='(f6.4)')
	endif

	sps = rep_missing_pix(sps,miss=ibad)	; replace missing pixels with average i-1 & i+1 frame
	if miss ne 0 then dinfo.com=dinfo.com+'/missing_pix '
	dualsp_calib1,sps,drk,fltl,fltr,ap,spsl,spsr;,/ver	; dark, two-sp, aligh, flat
	chk_1st_frame,spsl,spsr,ap,dinfo,xprof=xprof		; xprof[nxp,nn]  - wl-ave. intensity along slit & modulation
	if dinfo.xalign then begin
		xalign_sps,spsl,spsr,xprof,dx=dx		;  align sps[*,*,nn] in slit direction
		dinfo.com = dinfo.com+'/xalign '
	endif
	com = 'demo'
	s0 = dualsp_demodulate(spsl,spsr,ap,dinfo,sl=sl,sr=sr,div='c') ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
	dispiquvr,s0,bin=bin,pmax=pmax,/ialog,wid=0,title=com+' only'
	filename_sep,file1,di,fnam,ex
	win2gif,gifdir+fnam+'_s.gif'

	s1 = correct_DSTpol(s0,dinfo.wl0,dst,sc=sc,mm=mm,pars=pcal.pars)
	com = com + ' & mmdst [pcal]'

	case dinfo.correct_I2quv of
		-1: begin
			s2 = s1
			for iii=0,2 do s2[*,*,iii+1] -= s2[*,*,0] * my_i2quv[iii]
			com = com + ' & I2quv [from my_i2quv]'
		end
	   1: begin
		s2 = correct_Icrosstk(s1, coeffs=pcal.i2quv) 
		com = com + ' & I2quv [from pcal]'
		end
	   2: begin
		s2 = correct_Icrosstk(s1,/get_coeffs, coeffs=i2quv) 
		pcal.i2quv = i2quv
		print,i2quv
		com = com + ' & I2quv [from data]'
		end
	   else: s2 = s1
	endcase
	s = correct_QUVconti(s2,ap.yc)
	com = com + ' & QUVconti.'
	;sr = correct_Vcrosstk(s3, coeffs=v2qu)
	dispiquvr,s,bin=bin,pmax=pmax,/ialog,wid=3,title=com ;
	xyouts,10,10,string(j,form='(i3)')+': '+fnam,/dev,chars=3,col=0
	win2gif,gifdir+fnam+'_sr.gif'

	imgsize,s,nxp,nyp,n5
	outfile=outdir+fnam+'.sav'
	save,s,pcal,dinfo,h,xprof,dx,file=outfile
	print,'s,pcal,dinfo,h,xprof,dx  saved in ',outfile
	;win2gif,workdir+outdir+'iquvimg/'+fnam+'.gif'
endfor

stop

wlident:
;***** wavelength identification *************************** 
print,'>> identify wavelength <<'
print,'restoring ',cal.flat
restore,cal.flat	; fltl & fltr[nxp,ny], avfltl & avfltr[nxp,ny],fltspl & fltspr[nxp,ny]
imgsize,fltspl,nxp,nyp,n
iprof = transpose(fltspl[nxp/2,*,0]) 
if dinfo.wl_order eq 1  then iprof1=reverse(iprof) else iprof1=iprof
window,2
wl = wlident(iprof1, dinfo.wl0, dinfo.wl_range)
if dinfo.wl_order eq 1 then wl=reverse(wl)

save,iprof,wl,file=cal.wl
print,'wl[*] saved in ',cal.wl

stop

pltprof:
;***************  plot profiles *****************
print,'>> plot Stokes profiles <<'
diri = path.workdir+path.outdir
savfile1 = dialog_pickfile(path=diri,filter='*.sav')
restore,savfile1	;-> sr[*,*,4]
restore,cal.wl	;-> wl,iprof
iprof0 = iprof
imgsize,s,nxp,nyp,nn
bin = binfact(sr)
filename_sep,savfile1,di,fnam,ex

dispiquvr,s,bin=bin,pmax=pmax,wid=0;,/ialog
nx2 = nxp/bin &	ny2=nyp/bin
dd = (!d.x_size-nx2*4)/5

window,2,xs=1000,ys=700
!err=0
while !err ne 4 do begin	; 4=right button
	wset,0
	loadct,0
	cursor,px,py,3,/dev
	if !err eq 4 then break
	ix1 = (px-dd) mod (nx2+dd)
	for j=0,3 do plots,(dd+ix1)*[1,1]+(dd+nx2)*j,[0,ny2],line=1,/dev
	wset,2
	wshow,2
	stretch,255,0
	plotiquv,s,wl=wl,title=fnam,ix=ix1*bin,worder=dinfo.wl_order,pmax=pmax;,bin=binp
endwhile

iquvmap:
;***************  show iquvmap *****************
files = findfile(path.workdir+path.outdir+'*.sav')
nf = n_elements(files)
restore,files[dinfo.nstep/2]	; -> sr[*,*,4],pacal
restore,cal.wl	; wl[]
imgsize,s,nx,ny,n4
smap = fltarr(nx,dinfo.nstep,4)
sra = fltarr(nx,ny,4,dinfo.nstep)
for i=0,dinfo.nstep-1 do begin
	restore,files[i]
	sra[*,*,*,i] = s
endfor
bin = binfact(s)
nx2 = nx/bin &	ny2 = ny/bin
dispiquvr,s,bin=bin,pmax=pmax,/ialog,wid=0,dd=dd
cursor,px,py,3,/dev
ix1 = (px-dd) mod (nx2+dd) &	j1 = (py-dd)
for i=0,3 do begin
	x0 = dd+i*(nx2+dd)
	plots,x0+[0,nx2],dd+j1+[0,0],line=1,/dev
endfor

for i=0,dinfo.nstep-1 do begin
	print,i,'  ',files[i]
	restore,files[i]
	smap[*,i,*] = s[*,j1*bin,*]
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
	plotiquv,s,wl=wl,title=fnam,ix=ipos,worder=dinfo.wl_order,pmax=pmax,bin=1
endwhile








