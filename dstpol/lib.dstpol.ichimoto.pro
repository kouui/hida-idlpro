;  dst_pollib
;	2021.10.06	k.i.
;	2021.12.05	k.i.
;	2021.12.06	k.i.	get_dstmm,  cp update, +Q as perpendicular to VS slit
;	2021.12.28	k.i.	dispmaps
;	2022.02.24	k.i.	dinfo.camera
;	2022.02.28	k.i.,u.k.	rolling shutter = 10ms
;	2022.03.01	k.i.	rm_badframe(), rep_badframe(), correct_QUVconti()
;	2022.03.13	k.i.	rep_missing_pixc, chk_1st_frame, xalign_sps
;	2022.03.18	k.i.	dinfo.com
;	2022.04.07	k.i.,u.k.  get_dstmm use dst.pos,  cal structure
;	2022.04.08	k.i.,u.k.  yrange key in correct_Icrosstk(), dinfo.div,  iref keyword in plotiquv
;	2022.04.10	k.i.,u.k.  ap.icl, icr set only in align measure
;	2022.04.17	k.i.,u.k.  missing pix for count >1
;	2022.04.26	k.i.  	print_pcal
;	2022.04.29	k.i.,u.k.  - sign of U&V in demod. for reverse WP rotation, modu.ampl.& del mal/2, expo.effect
;	2022.04.30	k.i.,u.k.  if !d.x_size ne xs then window   in dispiquvr
;	2022.05.02	k.i.,u.k.  th_offset for ORCA, dispiquvr check window, move up rm_badframe
;	2022.05.06	k.i.,u.k.  mk_drkflt, if nflt eq 0 savedark & return
;	2022.05.06	u.k.       copy from /home/ichimoto/idlpro/hida/dstpol/dst_pollib.pro; 
;                             added `@lib_dstpol_para` to the end
;   2022.05.22  u.k.    dinfo_st and correct_Icrosstk added xrange,yrange; 


;********************************************************************
function arr2deg,dd
	rad = (dd[0]+dd[1]/60.+dd[2]/3600.)
	return,rad
end

;********************************************************************
function dst_st,dstarr

rad=!pi/180.

ha = (dstarr[0,0]+dstarr[1,0]/60.+dstarr[2,0]/3600.)/24.*2*!pi	; [hh,mm,ss] -> rad
if ha gt !pi then ha = ha-2*!pi

dst = {dst_st, $
	zd:	arr2deg(dstarr[*,1])*rad, 	$ ; zenith distance, (rad)
	ha:     ha, 				$ ; hour angle, (rad)
	az:     arr2deg(dstarr[*,9])*rad, 	$ ; azimuth, (rad)
	r: 	dstarr[1,2]+dstarr[2,2]/60., 	$ ; radius,  [min]
	p: 	arr2deg(dstarr[*,3]), 		$ ; polar angle,  [deg]
	incli:  arr2deg(dstarr[*,4])*rad,	$ ; inclination, (rad)
	pos: 	'WEST',				$ ; telescope position, string, 'EAST' or 'WEST'
	ga:	arr2deg(dstarr[*,5]), 		$ ; grating angle, [deg] 
	vr:	arr2deg(dstarr[*,7]), 		$ ; vdr encoder,   [deg]
	hr:	arr2deg(dstarr[*,8]) 		$ ; hdr encoder,  [deg]
	}
if dstarr[0,10] eq 0 then dst.pos = 'EAST'

return,dst
end

;********************************************************************
function read_dstfits,file,h,camera=camera,dst_status=dst,tim=tim,nslice=nslice

img = float(uint(readfits(file,h,nslice=nslice)))
imgsize,img,nx,ny,nn
ex = fits_keyval(h,'EXTEND')
if strcompress(ex,/remove_all) eq 'T' then begin
	dstarr = readfits(file,exten=1)	; dstarr[3,11]
	dst = dst_st(dstarr)
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
	box = double(asp[x-w1:x+w1,y-w1:y+w1])
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

x = dindgen(nx)

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
dfit = 3	; half interval of line fit
xhs = dblarr(ny)
xhf = dblarr(nh,ny)
y = dindgen(ny)
kb=2	; order of ine bottom fit 
for i=0,nh-1 do begin
	ix00 = xh[i]
	for j=0,ny-1 do begin
		xhs[j] = x0fit(sp[*,j],ix00,dfit,k=kb, z0=xz1)
	endfor
	cxh = poly_fit(y,xhs,k)
	xhf[i,*] = poly(y,cxh)
endfor

return,xhf

end

;********************************************************************
function yl_fit,sp,yl,k=k
;  polynomial fit for absorption lines

if not keyword_set(k) then k=2
s=size(sp) &	nx=s[1] &	ny=s[2]
nl = n_elements(yl)
dfit = 7	; half width of line fit
kb=2	; order of line bottom fit
yls = dblarr(nx)
ylf = dblarr(nl,nx)
x = dindgen(nx)
for j=0,nl-1 do begin
	ix00 = yl[j]
	for i=0,nx-1 do begin
		yls[i] = x0fit(transpose(sp[i,*]),ix00,dfit,k=kb, z0=xz1)
	endfor
	cyl = poly_fit(x,yls,k)
	ylf[j,*] = poly(x,cyl)
endfor

return,ylf

end

;********************************************************************
function rm_badframe,imgs,miss=count	; remove frames with missing pixels

	imgsize,imgs,nx,ny,nn
	mm = intarr(nn)
	for i=0,nn-1 do mm[i] = min(imgs[*,*,i])
	ii = where(mm ne 0, count)
	miss = nn-count
	if count lt nn then begin
		print,nn-count,' frames contain 0'
		imgs = imgs[*,*,ii]
	endif
	return,imgs
end

;********************************************************************
pro mk_drkflt, darkfiles,flatfiles,drk,avflt, flatdark=flatdark,savfile=savfile,hd=hd,hf=hf
; make dark & flat average  ->  drk[nx,ny],  avflt[nx,ny,nn]

	drks = read_dstfits(darkfiles[0],hd,cam=cam,dst=dst,tim=tim)
	imgsize,drks,nx,ny,nnd
	drks = rm_badframe(drks)
	drk = rebin(drks,nx,ny,1)
	nflt = n_elements(flatfiles)
	if nflt eq 0 then begin
		if keyword_set(savfile) then begin
			save,drk,hd,file=savfile.dark
			print,'dark saved in ',savfile.dark
		endif
		return
	endif
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

;********************************************************************
pro dualsp_align,asp,ap,spla,spra,measure = measure, kfit=kfit

;  asp[nx,ny]	- full spectra, with hairlines for measure
;  ap		- align param structure for input or return (measure)
;  spla,spra[nxp,ny]  - left & right spectra after alignment
;  measure 	- if set, set calib.params
;  kfit		- order of fitting polinomial for absorption lines and hairlines


imgsize,asp,nx,ny
if keyword_set(measure) then begin
	if not keyword_set(kfit) then kfit=3
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
	print,nh,' hazir lines x=',xhl_l
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
	kf = kfit	; order of polinomial
	xhf_l = xh_fit(spl,xhl,k=kf)
	for i=0,nh-1 do draw,(xhf_l[i,*]+ix1)*r,indgen(ny)*r,color=200
	xhf_r = xh_fit(spr,xhl,k=kf)
	for i=0,nh-1 do draw,(xhf_r[i,*]+ix1+ddx)*r,indgen(ny)*r,color=200

	;--- horizontal absorption line fit --
	kf = kfit	; order of polinomial
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
	ap = { $ ; psp_align_parm, $
		filename: '', 	$ ; filename of heirline image
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
		ylf_l:	ylf_l,	$ ; absorption line y-pos fit in left sp., [nl,nx]
		ylf_r:	ylf_r,	$ ;        "                  in right sp., [nl,nx]
		ylt:	ylt,	$ ; target absorption line y-pos in both sp [nl]
		icl:	0., 	$ ; continuum count of left sp.
		icr:	0.	$ ;     "              right sp.
		}	


endif else begin
	;--- separate spl & slr ---
	iddx = round(ap.ddx)
	spl = asp[ap.ix1:ap.ix2,0:ny-1]
	spr = asp[ap.ix1+iddx:ap.ix2+iddx,0:ny-1]
	spr = imgshift(spr,0,-ap.ddy)
endelse

;--- warping spectra ---
splw = spwarp1(spl,ap.xhf_l,ap.xht)
sprw = spwarp1(spr,ap.xhf_r,ap.xht)
spla = spwarp1(splw,ap.ylf_l,ap.ylt,/horiz)
spra = spwarp1(sprw,ap.ylf_r,ap.ylt,/horiz)
if keyword_set(measure) then begin
	ap.icl = median(spla[*,ap.yc[0]:ap.yc[1]])
	ap.icr = median(spra[*,ap.yc[0]:ap.yc[1]])
endif

end

;********************************************************************
function rep_missing_pix,imgs,miss=count	
; replace missing pixels (value=0) with average of i-1 & i+1 frame

	imgsize,imgs,nx,ny,nn
	mm = lonarr(nn)
	for i=0,nn-1 do begin
		ii = where(imgs[*,*,i] eq 0, n0)
		mm[i] = n0
	endfor
	ibad = where(mm gt 1, count)
	if count le 1 then return,imgs
	print,'frames of missing pixels: ',ibad
	miss = ibad
	for i=0,count-1 do begin
		ib = ibad[i]
		imgb = imgs[*,*,ib]
		ii = where(imgb eq 0)
		case ib of 
		       0: imgt = imgs[*,*,1] 
		    nn-1: imgt = imgs[*,*,nn-2] 
		    else: imgt = (imgs[*,*,ib-1]+imgs[*,*,ib+1])/2 
		endcase
		imgb[ii] = imgt[ii]
		imgs[*,*,ib] = imgb
	endfor
	return,imgs
end

;********************************************************************
pro chk_1st_frame,spsl,spsr,ap,dinfo,xprof=xprof,conti=conti
; replace 1st frame by a frame 1rot later if its level is unrelistic
; return xprof[nxp,nn]   -  wl-average or continuum (if /conti) intensity along slit & modulation

	imgsize,spsl,nxp,ny,nn
	if keyword_set(conti) then begin
		xprofl = reform(rebin(spsl[*,ap.yc[0]:ap.yc[1],*],nxp,1,nn),nxp,nn)
		xprofr = reform(rebin(spsr[*,ap.yc[0]:ap.yc[1],*],nxp,1,nn),nxp,nn)
	endif else begin
		xprofl = reform(rebin(spsl,nxp,1,nn),nxp,nn)
		xprofr = reform(rebin(spsr,nxp,1,nn),nxp,nn)
	endelse
	xprof = xprofl + xprofr/dinfo.rlr
	ib = reform(rebin(xprof,1,nn),nn)
	ibm = mean(ib[1:nn-1])
	rms = sqrt(total((ib[1:nn-1]-ibm)^2)/(nn-1))
	if ib[0] gt ibm + 5*rms then begin
		print,'1st frame replaced'
		dinfo.com = dinfo.com+'/bad_1st_frame '
		i2 = round(nn/dinfo.nrot)
		spsl[*,*,0] = spsl[*,*,i2]
		spsr[*,*,0] = spsr[*,*,i2]
	endif

end


;********************************************************************
pro xalign_sps,spsl,spsr,xprof,dx=dx	;  align spsl & spsr[*,*,nn] in slit direction

	imgsize,spsl,nxp,ny,nn
	print,'align spsl & spsr[*,*,nn] in slit direction..'
	window,12,xs=nxp,ys=nn*2+3
	tvscl,xprof
	xprofp = xprof[nxp*0.1:nxp*0.9,*]	; avoid both sides
	dx = fltarr(nn)
	for i=0,nn-1 do begin
		dx[i] = ccpeak1d(xprof[*,i],xprof[*,0],cc=cc)
	endfor
	dx = dx-mean(dx)
	xprof_s = xprof
	for i=0,nn-1 do begin
		for j=0,ny-1 do begin
			spsl[*,j,i] = rshift(spsl[*,j,i],-dx[i])
			spsr[*,j,i] = rshift(spsr[*,j,i],-dx[i])
		endfor
		;spsl[*,*,i] = imgshift(spsl[*,*,i],-dx[i],0)
		;spsr[*,*,i] = imgshift(spsr[*,*,i],-dx[i],0)
		xprof_s[*,i] = rshift(xprof[*,i],-dx[i])
	endfor
	tvscl,xprof_s,0,nn+3
end

;********************************************************************
pro dispiquvr,i,q,u,v,r,bin=bin,pmax=pmax,coms=coms,ialog=ialog,wid=wid,ipos=ipos,dd=dd, $
	isigma=isigma,title=title,get_ipos=get_ipos,get_box=get_box

if not keyword_set(bin) then bin=2
if not keyword_set(pmax) then pmax=0.01
if not keyword_set(coms) then coms=['I','Q','U','V','R']
if not keyword_set(dd) then dd=2

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
xs=nx2*nsp+dd*(nsp+1)
ys=ny2+dd*2
device,window_state=wstat
if (wstat[wid] eq 1) then wset,wid
if (wstat[wid] eq 0) or (!d.x_size ne xs) or (!d.y_size ne ys) then window,wid,xs=xs,ys=ys,title=title $
else wset,wid

intens=congrid(ss[*,*,0],nx2,ny2)
if keyword_set(isigma) then sgm=isigma else sgm=[-3,1]
if keyword_set(ialog) then tvscls,alog(intens),x0,dd $
else tvscls,intens,x0,dd,sgm=sgm ;vmax=median(ss[*,*,0])*1.2
if n_elements(coms) eq nsp then xyouts,10,ny2-30,coms[0],chars=3,charthick=2,/dev
for j=1,nsp-1 do begin
	x0 = dd+(nx2+dd)*j
	tvscls,congrid(ss[*,*,j],nx2,ny2),x0,dd,vmin=-pmax,vmax=pmax
	if n_elements(coms) ge nsp then begin
		xyouts,x0+10,ny2-30,coms[j],chars=3,charthick=2,/dev
	endif
endfor
if n_elements(ipos) ne 0 then begin
	for j=0,nsp-1 do begin
		x0 = dd+(nx2+dd)*j
		draw,x0+ipos/bin*[1,1],dd+[0,ny2],line=1
	endfor
endif
if keyword_set(get_ipos) then begin
	wshow
	print,'select x-position'
	cursor,px,py,3,/dev
	ix1 = (px-dd) mod (nx2+dd) &	j1 = (py-dd)
	for j=0,nsp-1 do begin
		x0 = dd+j*(nx2+dd)
		plots,x0+ix1*[1,1],dd+[0,ny2],line=1,/dev
	endfor
	ix1 = ix1*bin
	get_ipos = ix1
endif
if keyword_set(get_box) then begin
	wshow
	print,'select region...'
	wdok,'Select a region to correct I->QUV crosstalk',xpos=500,ypos=400
	box_cur1, x0c, y0c, nxc, nyc
	x0c = (x0c-dd) mod (nx2+dd) &	y0c = (y0c-dd)
	get_box = [x0c, y0c, nxc, nyc]
	for j=0,nsp-1 do begin
		x0 = dd+j*(nx2+dd) + x0c
		plots,x0+nxc*[0,1,1,0,0],dd+y0c+[0,0,nyc,nyc,0],line=1,/dev, col=0
	endfor
	get_box = get_box*bin
endif

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
th = findgen(nn)/nn * nrot*2.*!pi + th_offset/180.*!pi

if n_elements(roll_shutter) eq 2 then begin
	print,'roll shutter corrected : dth1, y0 = ',roll_shutter
	dth1 = roll_shutter[0]	; deg./line
	jy0 = roll_shutter[1]
	dths = [reverse(findgen(jy0)*dth1),findgen(ny-jy0)*dth1]
	dth = fltarr(nxp,ny)
	for i=0,nxp-1 do dth[i,*] = dths
	dth = dth/180.*!pi
endif else begin
	dth = 0.
endelse

s = fltarr(nxp,ny,5)
for i=0,nn-1 do begin	;  minus sign of U & V for reverse rotation of WP 
	s[*,*,0] = s[*,*,0] + sps[*,*,i]			; I
	s[*,*,1] = s[*,*,1] + sps[*,*,i] *cos(4*(th[i]+dth))	; Q
	s[*,*,2] = s[*,*,2] - sps[*,*,i] *sin(4*(th[i]+dth))	; U  
	s[*,*,3] = s[*,*,3] - sps[*,*,i] *sin(2*(th[i]+dth))	; V
	s[*,*,4] = s[*,*,4] + sps[*,*,i] *cos(2*(th[i]+dth))	; R
endfor
s = s/nn

return,s

end

;********************************************************************
function dualsp_demodulate,spsl,spsr,ap,dinfo,sl=sl,sr=sr
;  ap	- alignment params.
;  dinfo - data info.

if not keyword_set(pmax) then pmax=0.02
if not keyword_set(bin) then bin=2 

imgsize,spsl,nxp,ny,nn
print,'Demodulation... cemare='+dinfo.camera+',  th_offset='+string(dinfo.th_offset,form='(f6.2)')+'deg.'
camera = dinfo.camera
;camera = ''	; roll_shutter off
if camera eq 'ORCA4' then begin
	dthdt = 360./dinfo.rotp	; deg./sec
	dth1 = dthdt*0.010/(2048/2/dinfo.bin)	; change of wp angle in a 1 line reading in deg., 1frame=10ms
	if ap.ddy lt 0 then jy0 = 1024/dinfo.bin
	roll_shutter = [dth1,jy0]
	print,'correct rolling shutter...'
endif else roll_shutter = 0

if camera eq 'ORCA4' then roll_shutter[1] = jy0 + round(ap.ddy)
sl = sp_demodulate(spsl,dinfo.nrot, dinfo.th_offset, roll_shutter=roll_shutter)
if camera eq 'ORCA4' then roll_shutter[1] = jy0
sr = sp_demodulate(spsr/dinfo.rlr, dinfo.nrot,dinfo.th_offset, roll_shutter=roll_shutter)

case dinfo.div of 
    'i': begin ; divide by intensity
	for i=1,4 do begin
		sl[*,*,i] = sl[*,*,i]/sl[*,*,0]
		sr[*,*,i] = sr[*,*,i]/sr[*,*,0]
	endfor
 	end
    'c': begin ; divide by continuum
	;for i=0,nxp-1 do begin	; each spectrum  -> not good to do before pol.cal
	;	icl = mean(sl[i,ap.yc[0]:ap.yc[1],0])
	;	icr = mean(sr[i,ap.yc[0]:ap.yc[1],0])/dinfo.rlr
	;	sl[i,*,1:4] = sl[i,*,1:4]/icl
	;	sr[i,*,1:4] = sr[i,*,1:4]/icr
	;endfor
	sl = sl/ap.icl
	sr = sr/(ap.icr/dinfo.rlr)
	end
   else: 
endcase

print,'combine two spectra..'
s = sl

dth = dinfo.nrot/nn *2 *!pi
mal = (1-cos(dinfo.del/180*!pi))/2. *1/(2*dth)*sin(2*dth)
mac = sin(dinfo.del/180*!pi) *1/dth *sin(dth)
s[*,*,0] = (sl[*,*,0]+sr[*,*,0])/2
s[*,*,1:2] = (sl[*,*,1:2]-sr[*,*,1:2])/mal 	; divide mod. ampl. to get Q,U
s[*,*,3:4] = (sl[*,*,3:4]-sr[*,*,3:4])/mac 	; divide mod. ampl. to get V

if dinfo.wl_order eq 1 then begin	; swap l and r
	print,'reverse left and right, change sign of Q'
	s[*,*,1] = -s[*,*,1]
	s[*,*,2] = -s[*,*,2]
endif

;-- rotate to align +Q in slit direction
phi2 = 2.*dinfo.rotang/180*!pi
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
function correct_Icrosstk,s,get_coeffs=get_coeffs,coeffs=coeffs,xrange=xr,yrange=yr,correct=correct
;-- I -> Q,U,V & bias correction, if needed, do after DST Mueller matrix is applied --
;  s[*,*,5]
;  coeffs[2,4]    P' = P -c0*I - c1
imgsize,s,nx,ny,nn
if (not keyword_set(yr)) or (yr[0]+yr[1] eq 0) then yr=[0,ny-1]
if (not keyword_set(xr)) or (xr[0]+xr[1] eq 0) then xr=[0,nx-1]

if keyword_set(get_coeffs) or not keyword_set(coeffs) then begin
	coeffs = fltarr(2,nn-1)
	for i=0,nn-2 do coeffs[*, i]= poly_fit(s[xr[0]:xr[1],yr[0]:yr[1],0],s[xr[0]:xr[1],yr[0]:yr[1],i+1],1)
endif

s2 = s
dic = ['','Q','U','V']
if keyword_set(correct) then begin
	for i=1,3 do begin
		pos = where(dic[i] eq correct, count)
		if count then begin
			coe = coeffs[*,i-1]
			s2[*,*,i] = s[*,*,i] - coe[1]*s[*,*,0] - coe[0]
			print,'I -> ',dic[i],'  ',coe
		endif	
	endfor
endif else begin
	for i=1,nn-1 do begin
		coe = coeffs[*,i-1]
		s2[*,*,i] = s[*,*,i] - coe[1]*s[*,*,0] - coe[0]
		print,'I -> ',dic[i],'  ',coe
	endfor
endelse

return,s2

end

;********************************************************************
function correct_Vcrosstk,s,get_coeffs=get_coeffs,coeffs=coeffs
;-- V -> Q,U correction, if needed, do after DST Mueller matrix is applied --
;  s[*,*,4]
;  coeffs[2,2]    Q' = Q - c0*V - c1
imgsize,s,nx,ny,nn

if keyword_set(get_coeffs) or not keyword_set(coeffs) then begin
	coeffs = fltarr(2,2)
	for i=0,1 do coeffs[*, i]= poly_fit(s[*,*,3],s[*,*,i+1],1)
endif
s2 = s
for i=0,1 do begin
	coe = coeffs[*,i]
	s2[*,*,i+1] = s[*,*,i+1] - coe[1]*s[*,*,3] - coe[0]
endfor

return,s2

end

;********************************************************************
function correct_QUVconti,s,yc
; shift QUV continuum to zero

imgsize,s,nx,ny,n4
sb = s
for i=1,3 do begin
	conti = rebin(s[*,yc[0]:yc[1],i],nx,1,1)
	for j=0,ny-1 do sb[*,j,i] = s[*,j,i] - conti
endfor

return,sb

end

;********************************************************************
pro dualsp_calib1,sps,drk,fltl,fltr,ap,spsl,spsr,verbose=verbose	; dark, two-sp, aligh, flat
;  sbt dark, extract two sp, align & flat
;  sps,drk [nx,ny,nn], fltl,fltr [nxp,ny] or [nxp,ny,nn] -->  spsl,spsr[nxp,nyp,nn]

imgsize,fltl,nxf,nyf,nnf
imgsize,sps,nx,ny,nn
nxp = ap.ix2-ap.ix1+1
;---  align & flat  --------
print,'dualsp_calib1: Extract two spectra & flat correction ...'
spsl = fltarr(nxp,ny,nn)  &	spsr = spsl
iddx = round(ap.ddx)
for i=0,nn-1 do begin
	sp1 = sps[*,*,i] - drk
	dualsp_align,sp1,ap,spl,spr
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
iddy = fix(abs(ap.ddy))
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

;********************************************************************
pro print_pcal,pcal,dinfo=dinfo,h=h

	if keyword_set(h) then date_obs=fits_keyval(h,'DATE_OB2') else date_obs=''
	print,'-----------------------------------------------------------------'
	if keyword_set(dinfo) then $
		print,date_obs,'   ',dinfo.telpos,'  wl=',dinfo.wl0
	print,'DST params	Xn        Tn        Xc        Tc        Sc'
	pi = pcal.pars_init &	pf = pcal.pars
	print,'Anan    :',pi.xn,pi.tn,pi.xc,pi.tc,pi.sc,form='(a10,5f10.5)'
	print,'adjusted:',pf.xn,pf.tn,pf.xc,pf.tc,pf.sc,form='(a10,5f10.5)'
	print,'-----------------------------------------------------------------'

end

;**********************************************************
function get_dstmm,wl0,dst,sc=sc,qin=qin,pars=pd
;  return DST Muelelr matrix
;	Sin +Q is in E-W direction of the plane of sky, Sout +Q is perpendicular to VS slit
;  wl0		- wavelength in A
;  telpos	- 'WEST' or 'EAST'
;  dst		- dst status structure

if not keyword_set(sc) then sc=0.	; scatter light fraction
if not keyword_set(qin) then qin=''	; return mm refering -Q = slit direction

pd = par_dst(wl0,dst.pos)
mm = mm_dst(dst.zd, dst.ha, dst.az, dst.incli, dst.pos, pd.xn, pd.tn, pd.xc, pd.tc, sc=sc, qin=qin)

return,mm

end


;********************************************************************
function correct_DSTpol,s,wl0,dst,sc=sc,qin=qin,mm=mm,pars=pars,get_pars=get_pars

;--- correct DST polarization ---
if keyword_set(pars) then begin
	;mm = mm_dst(dst.zd, dst.ha, dst.az, dst.incli, dst.pos, pars.xn, pars.tn, pars.xc, pars.tc, sc=sc, qin='slit')
        mm = update_mmdst(dst, pars.xn, pars.tn, pars.xc, pars.tc, pars.sc, th_vs=pars.th_vs)
endif else begin
	mm = get_dstmm(wl0,dst,sc=sc,qin='slit',pars=get_pars)
	;mm = muellermatrix_rot(!pi/4)## get_dstmm(wl0,dst,sc=sc,qin='slit',pars=get_pars)	; +Q to 45deg analyzer
	print,"using Anan's mm"
endelse

rmm = invert(mm)
;rmm[0,2]=-rmm[0,2]
s2 = s[*,*,0:3]
for j=0,3 do begin
	s2[*,*,j] = rmm[0,j]*s[*,*,0]
	for i=1,3 do s2[*,*,j] = s2[*,*,j] + rmm[i,j]*s[*,*,i]
endfor

return,s2

end

;********************************************************************
pro dispmaps,maps,coms,yfact=yfact,wid=wid,bin=bin,pmax=pmax,dd=dd
;  display 2D maps

if not keyword_set(wid) then wid=0
if not keyword_set(bin) then bin=1
if not keyword_set(yfact) then yfact=1
if not keyword_set(pmax) then pmax=0.05
if not keyword_set(dd) then dd=2

imgsize,maps,nx,ny,nn
nx2=nx/bin
ny2=ny*yfact/bin
window,wid,xs=dd*(nn+1)+nx2*nn,ys=dd*2+ny2
for i=0,nn-1 do begin
	kind=strmid(coms[i],0,1)
	x0=dd+(nx2+dd)*i &	y0=dd
	map1 = congrid(maps[*,*,i],nx2,ny2)
	case kind of
	  'I': tvscl,map1,x0,y0
	  'V': tv,(map1>(-pmax)<pmax + pmax)/2/pmax*255,x0,y0
	 else: tvscl,map1,x0,y0
	endcase
	xyouts,x0+5,ny2-20,coms[i],/dev,charsize=2
endfor

end


;********************************************************************
function binfact,arr

imgsize,arr,nxp,ny,nn
if nxp ge 800 then bin = 4 
if nxp ge 500 and nxp lt 800 then bin = 3 
if nxp ge 300 and nxp lt 500 then bin = 2
if nxp lt 300 then bin = 1

return,bin

end

;********************************************************************
pro plotiquv,s0,wl=wl0,ix=ix0,bin=bin,worder=wl_order,title=title0,pmax=pmax,ireference=iref0
; iref	- reference I-prof

if not keyword_set(bin) then bin=1
if not keyword_set(wl_order) then wl_order=0
if not keyword_set(pmax) then pmax=0.05
if not keyword_set(title0) then title0=''

imgsize,s0,nx,ny,n4
if keyword_set(ix0) then ix=ix0/bin else ix=nx/bin/2
nx2=nx/bin &	ny2=ny/bin &	ix=ix0/bin
sb = congrid(s0,nx2,ny2,4)
wl = congrid(wl0,ny2)
if keyword_set(iref0) then iref = congrid(iref0,ny2)
iprof = transpose(sb[ix,*,0])
qprof = transpose(sb[ix,*,1])	
uprof = transpose(sb[ix,*,2])	
vprof = transpose(sb[ix,*,3])	

if wl_order then begin	; reverse wavelength
	wl=reverse(wl)
	iprof=reverse(iprof)
	qprof=reverse(qprof)
	uprof=reverse(uprof)
	vprof=reverse(vprof)
	if keyword_set(iref0) then iref=reverse(iref)
endif

;stretch,255,0
wy1=0.20
box=[0.1,0.,.95,wy1]
yoff=[0,1,0,1]
blank=replicate(' ',10)
yr=pmax*[-1,1]
wln=wl/10
xr=[wln[0],wln[ny2-1]]
cs=2.2
title = title0+'  x='+string(ix0,form='(i3)')
plot,wln,iprof,pos=box+yoff*(0.1+wy1*3),/norm,xtickname=blank,chars=cs, $
	xr=xr,xstyle=1,ystyle=1,title=title,yr=[0,max(iprof)*1.1]
if keyword_set(iref0) then oplot,wln,iref
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


end

;****************************z**************************************************
function contizero1,prof0,cfit=cfit,iiex=iiex
; make continuum base zero by iterative fit upto 3rd poly
;  iiex  - data points excluded (line signal) 

prof = prof0
n = n_elements(prof)
prof = reform(prof,n)
x = findgen(n)

c1 = poly_fit(x,prof,1)
fit1 = poly(x,c1) &	prof = prof-fit1 &	rms1 = stddev(prof)
;plot,prof
ii1 = where(abs(prof) gt rms1*3) &	prof[ii1] = 0.
c2 = poly_fit(x,prof,2)
fit2 = poly(x,c2) &	prof = prof-fit2 &	rms2 = stddev(prof)
ii2 = where(abs(prof) gt rms2*3) &	prof[ii2] = 0.
c3 = poly_fit(x,prof,3)
fit3 = poly(x,c3) &	prof = prof-fit3 &	rms3 = stddev(prof)
ii3 = where(abs(prof) gt rms3*2) &	prof[ii3] = 0.
c4 = poly_fit(x,prof,3)
fit4 = poly(x,c4) &	prof = prof-fit4 &	rms3 = stddev(prof)

cfit = fit1 + fit2 + fit3 + fit4
iiex = [ii1,ii2,ii3]
iiex = iiex[uniq(iiex,sort(iiex))]

;plot,prof0,col=150
;oplot,cfit

return,prof0-cfit

end

;****************************z**************************************************
function contizero_s3d,s
;	s[*,*,4]
sc = s
imgsize,s,nx,ny,n4
for k=1,3 do begin
	for i=0,nx-1 do sc[i,*,k] = contizero1(s[i,*,k],iiex=iie)
endfor

return,sc

end

;********************************************************************
function path_st

	path = {dstpol_path, $	
		rootdir: '/mnt/HDD3TBn51/DST/sp/20220209/', $	; root directory of raw dataset
		obsdir: 'NaI5896/ar2/',			$	; holder of obs dataset
		caldatdir: 'calib/',			$ 	; holder of clib. data
		darkfile: '/dark_orca_50ms_*.fits',	$ 	; dark file path in caldir
		flatfile: 'flat_*.fits',		$ 	; flat file path in caldir
		flatdark: '',				$	; dark for flat in caldir if expo. is different 
		hairline: 'hair*.fits',			$	; hairline data in caldir 
		polarizer: 'pol*.fits',			$	; polarizer data in caldir
		obsdat: '*.fits',			$	; observation seq. data
		workdir: '/nwork/ichimoto/20220209_pol/',$	; output root directory in /nwork
		outdir: 'NaD/',				$	; directory for obs.data output in workdir
		calpardir: 'cal/',			$	; directory for saving calib.params in workdir
		calid : ''				$	; ID of calibration files, .sav
		}
	return,path
end


;********************************************************************
function calfile_st

	cal = {calibration_files, $	
		dark: '', 	$	; 
		flat_org: '',	$	; sum of flat image
		flat: '',	$ 	; flats for left & right spectra
		ap: '',		$ 	; alignment param. structure
		dinfo: '',	$ 	; data info. structure
		pcal: '',	$	; pol.calib. param structure 
		wl: ''		$	; wavelength array 
		}
	return,cal
end


;********************************************************************
function dinfo_st

	dinfo = {dstpol_obs, $	; data info.
		nstep: 9 ,		$ ; # of steps for 1 scan
		i_scan: 0 ,		$ ; scan # for testing cal.
		j_pos:  3 ,		$ ; position # in a scan for test
		wl0: 5893. ,		$ ; central wavelength [A]
		wl_range: 15.,		$ ; apporox. wavelength range [A] 	
		wl_order: 1.,		$ ; 0: top is red, 1: top is blue  -> left sp is +U or -U
		telpos:	'',		$ ; DST position,  'WEST' or 'EAST',  get from dst_info
		incli:  0.,		$ ; inclination if dst_info not available
		camera: '',		$ ; 'ORCA4', 'FLIR', 
		expo:	0.01,		$ ; exposure time, sec 
		bin:	1,		$ ; binning in obs.
		rotp:	2.,		$ ; rotation period of wp [sec]
		nrot:	2.,		$ ; # of wp rotation during accuisition
		th_offset: 38.,		$ ; offset angle of waveplate to eliminate R, deg.
		rotang:	45.,		$ ; angle between PBS ax and slit to make +Q perpendicular to slit
		rlr: 	0.,		$ ; throughtput ratio,  amp_r/amp_l
		del: 	0.,		$ ; retardation of WP in deg.
		div:	'c',		$ ; divide QUV by continuum 'c',  intensity 'i', and non ''
		xalign:	1,		$ ; if 1, align in slit direction for demodulation
		adj_dstpol: 1,		$ ; if 1, adjust DST pol.parms using Zeemen in sunspot 
		correct_I2quv: 1,	$ ; 0 - no correction, 1 - use i2quv[2,3],  2 - correct with own IQUV
		I2quv_xrange: [0,0],    $ ; x-range for correct_I2quv
        	I2quv_yrange: [0,0],    $ ; y-range for correct_I2quv
		com:	''		$ ; comments
		}
	return,dinfo
end

;********************************************************************
