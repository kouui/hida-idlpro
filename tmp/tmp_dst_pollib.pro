;  dst_pollib
;	2021.10.06	k.i.
;	2021.12.05	k.i.
;	2021.12.06	k.i.	get_dstmm,  cp update, +Q as perpendicular to VS slit

;********************************************************************
function read_dstfits,file,h,camera=camera,dst_status=dst,tim=tim,nslice=nslice

img = float(uint(readfits(file,h,nslice=nslice)))
imgsize,img,nx,ny,nn
ex = fits_keyval(h,'EXTEND')
if strcompress(ex,/remove_all) eq 'T' then begin
	dst = readfits(file,exten=1)	; dst[3,11]
endif
;status = {dst_status_param,           $ description	since 2021.9
;          ha : str2angle(words[0]),          $ hour angle,  [hh mm ss]
;          zd : str2angle(words[1]),          $ zenith distance,  [deg. min. sec.]
;          r  : str2angle(words[2]),          $ radius,  [- min sec]
;          p  : str2angle(words[3]),          $ polar angle,  [deg. min. sec]
;          i  : str2angle(words[4]),          $ inclination,  [deg. min. sec]
;          ga : str2angle(words[5]),          $ grating angle,  [deg. min. sec]
;          gn : words[9],                     $ grating number, ex. VS#1, HS#3  (not ready 2021.12)
;          vr : val2angle(float(words[7])),   $ vdr encoder,   [deg. min. sec]
;          hr : val2angle(float(words[8])),   $ hdr encoder,  [deg. min. sec]
;          az : str2angle(words[6]),          $ azimuth,  [deg. min. sec]
;          pos: words[10]                     $ telescope position, 0-East, 1-West, since 2021.10
;}

skipimg = 0	; no need skip image for FLIR, 2021.11.28
camera = fits_keyval(h,'CAMERA')
if strmid(camera,0,4) eq 'FLIR' then begin
	tims = readfits(file,exten=2)	; hh,mm,ss,ms
	imgsize,tims,n4,nn2
	if n4 eq 4 then begin
		ts=double(tims)
		tim = ts[0,*]*3600.+ts[1,*]*60+ts[2,*]+ts[3,*]/1000
		tim = reform(tim,nn2)
		if skipimg then begin	; discard first 2or3 image, add top images at last
			dt = shift(tim,-1)-tim
			dtm = mean(dt[nn/2:nn-2])
			dmy = max(abs(dt[0:nn-2]-dtm),im)
			im = im+2
			print,'dtmax at i=',string(im,form='(i2)'),',  first', $
				string(im+1,form='(i2)'),' images discarded..'
			img = [[[img[*,*,im+1:nn-1]]],[[img[*,*,im+1:im+1+im]]]]
			tim = [tim[im+1:nn-1],tim[nn-1]+dtm*(1+findgen(im+1))]
		endif
	endif
endif

return,img

end

;********************************************************************
function crosspoint,asp,r,w1=w1,dfit=dfit,msg=msg

	if not keyword_set(w1) then w1 = 20	; half width of box
	if not keyword_set(dfit) then dfit = 3  ; half width for fitting
	if keyword_set(msg) then print,msg
	cursor,x0,y0,/dev,/down
	x=x0/r &	y=y0/r
	box=asp[x-w1:x+w1,y-w1:y+w1]
	xprof=rebin(box,2*w1+1,1)
	ix00=w1
	xp = x0fit(xprof,ix00,dfit,k=2, z0=xz1)
	xp1 = x-w1+xp[0]
	yprof=transpose(rebin(box,1,2*w1+1))
	yp = x0fit(yprof,ix00,dfit,k=2, z0=yz1)
	yp1 = y-w1+yp[0]
	circle,xp1*r,yp1*r,5
	return,[xp1,yp1]
end

;********************************************************************
; warp1d.pro
;   refprm a 1D profile y(x) w/ control points xo[*] (original) and xt[*] (target)
;   
function warp1d,x,y,xo,xt,k=k,x2=x2

nc = n_elements(xo)
if not keyword_set(k) then k=nc-1

c = poly_fit(xo,xt,k)
x2 = poly(x,c)
;y2 = spline(x2,y,x)    ;  slow..
y2 = interpol(y,x2,x)

return,y2
end

;********************************************************************
function spwarp1,sp,xo,xt,horiz=horiz
;  1D reform sp[*,*] to make vertical (or horizontal) lines straight
;  sp[nx,ny]
;  xo[nn,ny]   -- original x, nn: # of control points
;  xt[nn]      -- target x

spw = sp
if keyword_set(horiz) then spw=transpose(spw)
s=size(spw) &	nx=s[1] &	ny=s[2]
nn = n_elements(xt)
;k1 = nn-1
k1 = fix(sqrt(nn))

x = findgen(nx)

for j=0,ny-1 do begin
	xo1 = xo[*,j]
	sp1 = [spw[*,j]]
	spw[*,j] = warp1d(x,sp1,xo1,xt,k=k1,x2=x2)
endfor

if keyword_set(horiz) then spw=transpose(spw)

return,spw

end

;********************************************************************
function spwarp,sp,xo,xt,horiz=horiz
;  1D reform sp[*,*] to make vertical (or horizontal) lines straight
;  sp[nx,ny]
;  xo[nn,ny]   -- original x, nn: # of control points
;  xt[nn]      -- target x

spw = sp
if keyword_set(horiz) then spw=transpose(spw)
s=size(spw) &	nx=s[1] &	ny=s[2]
nn = n_elements(xt)
k1 = fix(sqrt(nn))
x1 = xt
y0=replicate(0,nn)
for i=1,k1 do begin
	y0 = [y0,replicate(i,nn)]
	x1 = [x1,xt]
endfor
y1=y0

for j=0,ny-1 do begin
	x0 = xo[*,j]
	sp1 = [spw[*,j]]
	for i=1,k1 do begin
		x0 = [x0,xo[*,j]]
		sp1 = [[sp1],[spw[*,j]]]
	endfor
	polywarp,x0,y0,x1,y1,k1,kx,ky
	sp1w = poly_2d(sp1,kx,ky,/cubic)
	spw[*,j] = sp1w[*,0]
endfor

if keyword_set(horiz) then spw=transpose(spw)

return,spw

end

;********************************************************************
function xh_fit,sp,xh,k=k
;  polynomial fit for heirlines

if not keyword_set(k) then k=2
s=size(sp) &	nx=s[1] &	ny=s[2]
nh = n_elements(xh)
dfit = 3
xhs = dblarr(ny)
xhf = dblarr(nh,ny)
y = findgen(ny)
for i=0,nh-1 do begin
	ix00 = xh[i]
	for j=0,ny-1 do begin
		xhs[j] = x0fit(sp[*,j],ix00,dfit,k=k, z0=xz1)
	endfor
	cxh = poly_fit(y,xhs,k)
	xhf[i,*] = poly(y,cxh)
endfor

return,xhf

end

;********************************************************************
function yl_fit,sp,yl,k=k
;  polynomial fit for absorption lines

s=size(sp) &	nx=s[1] &	ny=s[2]
nl = n_elements(yl)
dfit = 7
yls = dblarr(nx)
ylf = dblarr(nl,nx)
x = findgen(nx)
for j=0,nl-1 do begin
	ix00 = yl[j]
	for i=0,nx-1 do begin
		yls[i] = x0fit(transpose(sp[i,*]),ix00,dfit,k=2, z0=xz1)
	endfor
	cyl = poly_fit(x,yls,k)
	ylf[j,*] = poly(x,cyl)
endfor

return,ylf

end

;********************************************************************
pro mk_drkflt, darkfiles,flatfiles,drk,avflt, flatdark=flatdark,savfile=savfile,hd=hd,hf=hf
; make dark & flat average  ->  drk[nx,ny],  avflt[nx,ny,nn]

	drks = read_dstfits(darkfiles[0],hd,cam=cam,dst=dst,tim=tim)
	imgsize,drks,nx,ny,nnd
	drk = rebin(drks,nx,ny,1)
	nflt = n_elements(flatfiles)
	flt1 = read_dstfits(flatfiles[0],hf,cam=cam,dst=dst,tim=tim)
	imgsize,flt1,nx,ny,nnf
	avflt = float(flt1)
	for i=1,nflt-1 do begin
		print,'flat-',string(i+1,nflt,form='(i3,"/",i3)'),'  ',flatfiles[i]
		flt1 = read_dstfits(flatfiles[i],hf,cam=cam,dst=dst,tim=tim)
		avflt = avflt+flt1
	endfor
	avflt = avflt/nflt
	if keyword_set(flatdark) then begin
		print,'flatdark..  ',flatdark[0]
		fdrks = read_dstfits(flatdark[0],hd,cam=cam,dst=dst,tim=tim)
		fdrk = rebin(fdrks,nx,ny,1)
	endif else fdrk = drk
	for j=0,nnf-1 do avflt[*,*,j] = avflt[*,*,j]-fdrk
	if keyword_set(savfile) then begin
		save,drk,hd,avflt,hf,file=savfile
		print,'dark & flat saved in ',savfile
	endif

end

;********************************************************************
pro dualsp_align,asp,cp,spla,spra,measure = measure

;  asp[nx,ny]	- full spectra, with hairlines for measure
;  cp		- param structure for input or return (measure)
;  spla,spra[nxp,ny]  - left & right spectra after alignment
;  measure 	- if set, set calib.params


imgsize,asp,nx,ny
if keyword_set(measure) then begin
	;-----  determin reference cross points, use average flat asp[*,*] ---
	imgsize,asp,nx,ny
	if nx eq 640 then window,xs=nx,ys=ny else window,xs=900,ys=900
	w1=30 &	dfit=4
	tvf,asp,r=r
	p0l = crosspoint(asp,r,w1=w1,dfit=dfit,msg='click a cross-point in left sp')
	wait,0.1	; unknown necessary
	p0r = crosspoint(asp,r,w1=w1,dfit=dfit,msg='click a cross-point in right sp')
	ddx = p0r[0]-p0l[0]	; separation of left-right spectra in x,y
	ddy = p0r[1]-p0l[1]
	print,'ddx=',ddx,'   ddy=',ddy
	xhl_l = p0l[0]
	yl_l = p0l[1]

	;-- select hairlines ---
	print,'select heirlines in left sp, right button for last one'
	while !err ne 4 do begin
		p = crosspoint(asp,r,w1=w1,dfit=dfit)
		xhl_l=[xhl_l,p[0]]
		print,'xy=',p
	endwhile

	;-- select absorption lines ---
	print,'select absorption lines in left sp'
	!err=1
	wait,0.5   ;  unknown necessary
	while !err ne 4 do begin
		p = crosspoint(asp,r,w1=w1,dfit=dfit)
		yl_l=[yl_l,p[1]]
		print,'xy=',p
	endwhile
	nh = n_elements(xhl_l)
	nl = n_elements(yl_l)
	print,nh,' heir lines x=',xhl_l
	print,nl,' absorption lines y=',yl_l

	;-- continuum interval ---
	print,'select continuum interval in left sp'
	wait,0.5
	box_cur1, x0c, y0c, nxc, nyc
	yc = fix((y0c+[0,nyc])/r)
	print,'continuum ys =',yc

	;--  spectral width ----
	xcut = asp[*,ny/2]
	;plot,xcut
	xd1=nx/2-nx/10 &	xd2=nx/2+nx/10
	dmy=min(smooth(xcut[xd1:xd2],10),i0)
	im = xd1+i0
	imax = max(xcut[0:im])
	thresh = 0.4
	margin = 10
	dmy=min(abs(xcut[0:xhl_l[0]-margin]-thresh*imax),ix1)
	xhlmx=max(xhl_l)
	dmy=min(abs(xcut[xhlmx+margin:im]-thresh*imax),ix2)
	ix2=round(xhlmx+5+ix2)

	;--- separate spl & slr ---
	iddx = round(ddx)
	spl = asp[ix1:ix2,0:ny-1]
	spr = asp[ix1+iddx:ix2+iddx,0:ny-1]
	spr = imgshift(spr,0,-ddy)
	nxp = fix(ix2-ix1+1)
	xhl = xhl_l - ix1
	yl = yl_l
	draw,[ix1,ix2,ix2,ix1,ix1]*r,[0,0,ny-1,ny-1,0]*r
	draw,([ix1,ix2,ix2,ix1,ix1]+ddx)*r,([0,0,ny-1,ny-1,0]+ddy)*r

	;--- vertical hairline fit --
	kf = 2	; order of polinomial
	xhf_l = xh_fit(spl,xhl,k=kf)
	for i=0,nh-1 do draw,(xhf_l[i,*]+ix1)*r,indgen(ny)*r,color=200
	xhf_r = xh_fit(spr,xhl,k=kf)
	for i=0,nh-1 do draw,(xhf_r[i,*]+ix1+ddx)*r,indgen(ny)*r,color=200

	;--- horizontal absorption line fit --
	xp = indgen(nxp)
	ylf_l = yl_fit(spl,yl,k=kf)
	for i=0,nl-1 do draw,(xp+ix1)*r,ylf_l[i,*]*r,color=200
	ylf_r = yl_fit(spr,yl,k=kf)
	for i=0,nl-1 do draw,(xp+ix1+ddx)*r,(ylf_r[i,*]+ddy)*r,color=200

	;--- target hairline & line pos. ---
	xht = dblarr(nh)
	for i=0,nh-1 do xht[i] = mean(xhf_l[i,*])
	ylt = dblarr(nl)
	for i=0,nl-1 do ylt[i] = mean(ylf_l[i,*])

	if 1 then begin
		window,2
		plot,xhf_l[0,*]-replicate(xht[0],ny),/nodata,yr=[-7,4],title='dx hairlines'
		for i=0,nh-1 do begin
			oplot,xhf_l[i,*]-replicate(xht[i],ny)
			oplot,xhf_r[i,*]-replicate(xht[i],ny),line=2
		endfor
		wset,0
	endif
	;--- store alignment, pol.cal.  params ---
	cp = { $ ; psp_align_parm, $
		datinfo: '', 	$ ; information (filename) of heirline image
		xhl_l:	xhl_l, 	$ ; hairline x-pos. in left sp.
		yl_l:	yl_l,	$ ; absorption line y-pos in left sp.
		yc:	yc,	$ ; continuum interval [2]
		ddx:	ddx,	$ ; x-separation of left-right sps.
		ddy:	ddy,	$ ; y-                 "
		ix1:	ix1,	$ ; left edge of left sp
		ix2:	ix2,	$ ; right edge of "
		xhf_l:	xhf_l,	$ ; hairline x-pos fit in left sp., [nh,ny]
		xhf_r:	xhf_r,	$ ;        "           in right sp., [nh,ny]
		xht:	xht,	$ ; target hairline x-pos. in both csp., [nh]
		ylf_l:	ylf_l,	$ ; absorption line y-pos fit in left sp., [nl,ny]
		ylf_r:	ylf_r,	$ ;        "                  in right sp., [nl,ny]
		ylt:	ylt,	$ ; target absorption line y-pos in both sp [nl]
		icl:	0., 	$ ; continuum count of left sp.
		icr:	0.,	$ ;     "              right sp.
		expo:	0.01,	$ ; exposure time, sec 
		nrot:	2.,	$ ; # of wp rotation during accuisition
		telpos:	'WEST',	$ ; DST position,  'WEST' or 'EAST'
		th_offset: 0.,	$ ; offset angle of waveplate to eliminate R, deg.
		wl_order: 0.,	$ ; 0: top is red, 1: top is blue  -> left sp is +U or -U
		rotang:	45.,	$ ; angle between PBS ax and slit to make +Q perpendicular to slit
		rlr: 	0.,	$ ; throughtput ratio,  amp_r/amp_l
		del: 	0.	$ ; retardation of WP in deg.
		}	

endif else begin
	;--- separate spl & slr ---
	iddx = round(cp.ddx)
	spl = asp[cp.ix1:cp.ix2,0:ny-1]
	spr = asp[cp.ix1+iddx:cp.ix2+iddx,0:ny-1]
	spr = imgshift(spr,0,-cp.ddy)
endelse

;--- warping spectra ---
splw = spwarp1(spl,cp.xhf_l,cp.xht)
sprw = spwarp1(spr,cp.xhf_r,cp.xht)
spla = spwarp1(splw,cp.ylf_l,cp.ylt,/horiz)
spra = spwarp1(sprw,cp.ylf_r,cp.ylt,/horiz)
cp.icl = median(spla[*,cp.yc[0]:cp.yc[1]])
cp.icr = median(spra[*,cp.yc[0]:cp.yc[1]])


end

;********************************************************************
pro dispiquvr,i,q,u,v,r,bin=bin,pmax=pmax,coms=coms,ialog=ialog,wid=wid

if not keyword_set(bin) then bin=2
if not keyword_set(pmax) then pmax=0.01

dd=2
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
window,wid,xs=nx2*nsp+dd*(nsp+1),ys=ny2+dd*2

intens=congrid(ss[*,*,0],nx2,ny2)
if keyword_set(ialog) then tvscls,alog(intens),x0,dd $
else tvscls,intens,x0,dd,sgm=[-3,1] ;vmax=median(ss[*,*,0])*1.2
if n_elements(coms) eq nsp then xyouts,10,ny2-30,coms[0],chars=2,/dev
for j=1,nsp-1 do begin
	x0 = dd+(nx2+dd)*j
	tvscls,congrid(ss[*,*,j],nx2,ny2),x0,dd,vmin=-pmax,vmax=pmax
	if n_elements(coms) eq nsp then begin
		xyouts,x0+10,ny2-30,coms[j],chars=3,charthick=2,/dev
	endif
endfor

end


;********************************************************************
function sp_demodulate,sps,nrot,th_offset,roll_shutter=roll_shutter
; sps[*,*,nn]
; nrot  	- # of wp rotation
; th_offset  	- offset angle of wp in deg.
; rollc_shutter[2] 
;     [0] dth1	- change of wp angle in a 1 line rolling shutter in deg.
;     [1] jy0	- center y of detector

imgsize,sps,nxp,ny,nn
th = findgen(nn)/nn * nrot*2*!pi + th_offset/180.*!pi

if n_elements(roll_shutter) eq 0 then begin
	dth = 0.
endif else begin
	dth1 = roll_shutter[0]
	jy0 = fix(roll_shutter[1])
	dth1 = [reverse(findgen(jy0)*dth1),findgen(ny-jy0)*dth1]
	dth = fltarr(nxp,ny)
	for i=0,nxp-1 do dth[i,*] = dth1
	dth = dth/180.*!pi
endelse

s = fltarr(nxp,ny,5)
for i=0,nn-1 do begin
	s[*,*,0] = s[*,*,0] + sps[*,*,i]			; I
	s[*,*,1] = s[*,*,1] + sps[*,*,i] *cos(4*(th[i]+dth))	; Q
	s[*,*,2] = s[*,*,2] + sps[*,*,i] *sin(4*(th[i]+dth))	; U
	s[*,*,3] = s[*,*,3] + sps[*,*,i] *sin(2*(th[i]+dth))	; V
	s[*,*,4] = s[*,*,4] + sps[*,*,i] *cos(2*(th[i]+dth))	; R
endfor
s = s/nn

return,s

end

;********************************************************************
function dualsp_demodulate,spsl,spsr,cp,sl=sl,sr=sr,div=div

if not keyword_set(pmax) then pmax=0.02
if not keyword_set(bin) then bin=2 

imgsize,spsl,nxp,ny,nn
print,'demodulation...'
sl = sp_demodulate(spsl,cp.nrot,cp.th_offset)
sr = sp_demodulate(spsr/cp.rlr,cp.nrot,cp.th_offset)

if not keyword_set(div) then div=''
case div of 
    'i': begin ; divide by intensity
	for i=1,4 do begin
		sl[*,*,i] = sl[*,*,i]/sl[*,*,0]
		sr[*,*,i] = sr[*,*,i]/sr[*,*,0]
	endfor
 	end
    'c': begin ; divide by continuum
	sl = sl/cp.icl
	sr = sr/cp.icr
	end
   else: 
endcase

print,'combine two spectra..'
s = sl
mal = 1-cos(cp.del/180*!pi)
mac = sin(cp.del/180*!pi)
s[*,*,0] = (sl[*,*,0]+sr[*,*,0])/2
s[*,*,1:2] = (sl[*,*,1:2]-sr[*,*,1:2])/mal 	; divide mod. ampl. to get Q,U
s[*,*,3:4] = (sl[*,*,3:4]-sr[*,*,3:4])/mac 	; divide mod. ampl. to get V

if cp.wl_order eq 1 then begin	; swap l and r
	print,'swapping L & R '
	s1 = s[*,*,1]
	s[*,*,1] = s[*,*,2]
	s[*,*,2] = s1
endif

;-- rotate to align +Q in slit direction
phi2 = 2.*cp.rotang/180*!pi
c2 = cos(phi2) &	s2 = sin(phi2)
q = s[*,*,1] &		u = s[*,*,2]
s[*,*,1] =  c2*q + s2*u
s[*,*,2] = -s2*q + c2*u

;-- I -> Q,U,V & bias correction, no need here if DST Mueller matrix is applied --
if 0 then begin
	for i=1,4 do begin
		coe = poly_fit(s[*,*,0],s[*,*,i],1)
		s[*,*,i] = s[*,*,i] - coe[1]*s[*,*,0] - coe[0]
		;s[*,*,i] = s[*,*,i] -s[*,*,0]*median(s[*,cp.yc[0]:cp.yc[1],i])/ic
	endfor
endif

return,s
end

;********************************************************************
function correct_Icrosstk,s
;-- I -> Q,U,V & bias correction, no need here if DST Mueller matrix is applied --
;  s[*,*,5]
imgsize,s,nx,ny,nn

	s2 = s
	for i=1,nn-1 do begin
		coe = poly_fit(s[*,*,0],s[*,*,i],1)
		s2[*,*,i] = s[*,*,i] - coe[1]*s[*,*,0] - coe[0]
	endfor

	return,s2

end


;********************************************************************
pro dualsp_calib1,sps,drk,fltl,fltr,cp,spsl,spsr,verbose=verbose	; dark, two-sp, aligh, flat
;  sbt dark, extract two sp, aligh & flat
;  sps,drk [nx,ny,nn], fltl,fltr [nxp,ny] or [nxp,ny,nn] -->  spsl,spsr[nxp,nyp,nn]

imgsize,fltl,nxf,nyf,nnf
imgsize,sps,nx,ny,nn
nxp = cp.ix2-cp.ix1+1
;---  align & flat  --------
print,'dualsp_calib1: Extract two spectra & flat ...'
spsl = fltarr(nxp,ny,nn)  &	spsr = spsl
iddx = round(cp.ddx)
for i=0,nn-1 do begin
	sp1 = sps[*,*,i] - drk
	dualsp_align,sp1,cp,spl,spr
	if keyword_set(verbose) then print,i,' /',nn
	if nnf eq nn then begin
		spsl[*,*,i] = spl/fltl[*,*,i]
		spsr[*,*,i] = spr/fltr[*,*,i]
	endif else begin
		spsl[*,*,i] = spl/fltl
		spsr[*,*,i] = spr/fltr
	endelse
endfor
;-- clip in y --
iddy = fix(abs(cp.ddy))
nyp = ny-2*iddy
spsl = spsl[*,iddy:ny-iddy-1,*]
spsr = spsr[*,iddy:ny-iddy-1,*]

if keyword_set(verbose) then begin ;-- display
	bin = 3
	nx2=nxp/bin &	ny2=nyp/bin
	spsl2 = congrid(spsl,nx2,ny2,nn)
	spsr2 = congrid(spsr,nx2,ny2,nn)
	window,xs=nx2*2,ys=ny2
	for i=0,nn-1 do begin
		tvscl,spsl2[*,*,i]
		tvscl,spsr2[*,*,i],nx2,0
	endfor
endif

end

;**********************************************************
function findfile_timeint,path,time1,time2
; return files in time interval 'hhmmss'
; filename must be '*****yyyymmdd.hhmmss.sss.*'

files=findfile(path)
nf=n_elements(files) &	hhmmss=strarr(nf)
for i=0,nf-1 do begin
	filename_sep,files[i],di,fnam1,ex
	ln = strlen(fnam1)
	hhmmss[i]=strmid(fnam1,ln-10,6)
endfor
ii=where(hhmmss ge time1 and hhmmss le time2)
files=files[ii]

return,files

end

;**********************************************************
function get_dstmm,wl0,telpos,dst,sc=sc,qin=qin
;  return DST Muelelr matrix
;	Sin +Q is in E-W direction of the plane of sky, Sout +Q is perpendicular to VS slit
;  wl0		- wavelength in A
;  telpos	- 'WEST' or 'EAST'
;  dst[3,11]	- dst status in fits extension

if not keyword_set(sc) then sc=0.	; scatter light fraction
if not keyword_set(qin) then qin=''	; return mm refering -Q = slit direction

;telpos='WEST'
pd = par_dst(wl0,telpos)
zd = (dst[0,1]+dst[1,1]/60.+dst[2,1]/3600.)/180.*!pi
ha = (dst[0,0]+dst[1,0]/60.+dst[2,0]/3600.)/24.*2*!pi	; [hh,mm,ss] -> rad
if ha gt !pi then ha = ha-2*!pi
az = (dst[0,9]+dst[1,9]/60.+dst[2,9]/3600.)/180.*!pi
incli = (dst[0,4]+dst[1,4]/60.+dst[2,4]/3600.)/180.*!pi

;mm = mm_dst(zd, ha, az, incli, telpos, 0., 0., 0., 0., sc=sc, qin=qin)
;stop
mm = mm_dst(zd, ha, az, incli, telpos, pd.xn, pd.tn, pd.xc, pd.tc, sc=sc, qin=qin)

return,mm

end




