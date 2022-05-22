;; history
;; 2022.05.06  k.u.  for demodulation and s0 caching with IDL_BRIDGE
;; 2022.05.20  k.u.  calculated dst -> raw dst,
;;                   dinfo -> dinfo_s0, to be saved in *.sav file,
;;                   do not save dinfo in *.s0.sav

;@mmdst
;@dst_pollib

;********************************************************************
PRO dualdemo_cache, file, outfile, drk, fltl, fltr, ap, dinfo, worktitle, s0=s0
	
	imgsize,fltl,nxp,ny,nn
	bin = binfact(fltl)

	sps = read_dstfits(file,h,cam=cam,dst=dst,tim=tim)
	; if (dst.zd eq 0. and dst.ha eq 0.) or (dst.zd gt 100.) then begin
	; 	date_obs = fits_keyval(h,'DATE_OB2')
	; 	dst = dst_st(calc_dstinfo(date_obs,dinfo.incli))
	; 	dst.pos = dinfo.telpos
	; 	print, worktitle, 'DSTinfo calculated from DATE_OB2,  ZD=', string(dst.zd,form='(f6.4)'), $
	; 		'  HA=',string(dst.ha,form='(f6.4)'),'  Azim=',string(dst.az,form='(f6.4)')
	; endif

	sps = rep_missing_pix(sps,miss=ibad)	; replace missing pixels with average i-1 & i+1 frame
	dualsp_calib1,sps,drk,fltl,fltr,ap,spsl,spsr;,/ver	; dark, two-sp, aligh, flat
	chk_1st_frame,spsl,spsr,ap,dinfo,xprof=xprof		; xprof[nxp,nn]  - wl-ave. intensity along slit & modulation
	if dinfo.xalign then xalign_sps,spsl,spsr,xprof,dx=dx		;  align sps[*,*,nn] in slit direction
	
	s0 = dualsp_demodulate(spsl,spsr,ap,dinfo,sl=sl,sr=sr) ; spls,sprs[nxp,nyp,nn],  sl,sr[nxp,nyp,5]
	if not keyword_set(dx) then begin
		sxprof=size(xprof)
		dx=fltarr(sxprof[2])
	endif
	; dinfo_s0 = dinfo
	; save, s0,dinfo_s0,h,dst,xprof,dx,file=outfile
	save, s0,h,dst,xprof,dx,file=outfile
	print, worktitle, 'saved as: ', outfile

END
;********************************************************************
PRO dualdemo_cache_files, files, outfiles, drk, fltl, fltr, ap, dinfo, $
		workid=workid,file_ap=file_ap,file_dinfo=file_dinfo

;	restore,cal.dark	; drk[nx,ny]
;	restore,cal.flat	; fltl[nxp,ny], fltr[nxp,ny]
	if keyword_set(file_ap) then restore, file_ap		; ap
	if keyword_set(file_dinfo) then restore, file_dinfo
	

	if not keyword_set(workid) then workid=0
	worktitle='[workid='+string(workid,form='(i1)')+'] '
	print, worktitle, 'Starting ...'

	nf1 = n_elements(files) & nf2 = n_elements(files)
	if nf1 ne nf2 then begin
		print, worktitle, 'confliction in number of files and outfiles'
		return
	endif
	
	for i=0, nf1-1 do begin
		file = files[i] & outfile = outfiles[i]
		print, worktitle, string(i+1,form='(i3)')+'/'+string(nf1,form='(i3)')+'  processing : ', file
		dualdemo_cache, file, outfile, drk, fltl, fltr, ap, dinfo, worktitle
	endfor

	print, worktitle, 'Completed.'

END
;********************************************************************
