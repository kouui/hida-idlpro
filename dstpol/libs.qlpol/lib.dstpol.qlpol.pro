;; history
;; 2022.05.05  k.u.

@import.dstpol.qlpol

;********************************************************************
function qlpol_st

	qlp = {qlpol_struct,      $
		alive:0,              $
		box: [0l,0l,0l,0l],    $
		bicrtk: [0l,0l,0l,0l], $
		is_realtime: 0,     $
		print_mm   : 0,     $
		stokes     : 'sun', $
		wave0: 10830.,      $
		wl_order: 1,        $
		pmax:0.02,          $
		telpos: 'WEST',     $
		incli:0.,           $
;;		mm:fltarr(4,4),     $
		par:par_dst(10830.,'WEST'), $
		expo:0.015,         $
		rotp:1.0           $
	}

;	for i=0,3 do qlp.mm[i,i]=1.

	return, qlp
end


;********************************************************************
function qlpol_stokes, img3d, date_obs, camera, expo, rotp, bin=bin, $ 
				orca_if=orca_if, show=show, winit=winit, replot=replot
	common qlpol_common, qlp, wdqlp, wid
	common qlpol_data_commom, s0, mm, date_obs0, very_dark, dark, dark_expo
	
	is_init=0b
	if keyword_set(winit) then is_init=1b

	if 0b then begin
		if (not is_init) and (not keyword_set(dark)) then begin
			print, '[qlpol] please take dark'
			return,1
		endif
		
		if (keyword_set(replot)) and (not keyword_set(dark)) then begin
			print, '[qlpol] no dark available, will not replot'
			return,1
		endif  
	endif

	if not keyword_set(replot) then begin
		
		qlp.expo=expo
		qlp.rotp=rotp
		print,'[qlpol] expo=',expo,'  rotp=',rotp

		dst = dst_st(calc_dstinfo(date_obs,qlp.incli))
		dst.pos = qlp.telpos
		par=qlp.par
		mm = UPDATE_MMDST(dst, par.xn, par.tn, par.xc, par.tc, par.sc)
		im3 = img3d[qlp.box[0]:qlp.box[2],qlp.box[1]:qlp.box[3],*]
		imgsize, im3, imnx,imny,imnn
		if (imnx gt 1024) or (imny gt 1024) then $
			im3 = rebin(im3, imnx/2, imny/2, imnn)

		very_dark=0b
		case STRUPCASE(camera) of
			'GOLDEYE' : thres=2500
			'ORCA4'   : thres=600
		endcase
		if max(im3) lt thres then begin
			print, '[qlpol] low camera counting'
			very_dark=1b
		endif

		;if not is_init is_init then begin ;; no dark subtraction
		if keyword_set(dark) then begin    ;; if there is dark set 
			imgsize, im3, imnx,imny,imnn
			imgsize, dark, dnx,dny
			if (imnx ne dnx) or (imny ne dny) then begin
				print, '[qlpol] conflicted size between data and dark'
				return, 1
			endif
			if dark_expo ne expo then begin
				print, '[qlpol] dark expo=',expo,'  dark expo=',dark_expo
				return, 1
			endif
			for kk=0,imnn-1 do im3[*,*,kk]=im3[*,*,kk]-dark
		endif
		s0 = img3d_demodulate(im3, qlp.expo, qlp.rotp, camera, $
								bin=bin, orca_if=orca_if, wl_order=qlp.wl_order)
		date_obs0=date_obs
	endif else begin
		if not keyword_set(date_obs0) then return, 1
		date_obs=date_obs0
	endelse
	if qlp.print_mm then begin
		print, '[qlpol] MM at ', date_obs
		print, mm
	endif
	
	if is_init then begin
		s3 = s0[*,*,0:3]
	endif else begin
		case qlp.stokes of
		'sun': begin
			rmm = invert(mm)
			s2 = s0[*,*,0:3]
			for j=0,3 do begin
				s2[*,*,j] = rmm[0,j]*s0[*,*,0]
				for i=1,3 do s2[*,*,j] = s2[*,*,j] + rmm[i,j]*s0[*,*,i]
			endfor
			if not very_dark then s3 = correct_Icrosstk(s2,yrange=[qlp.bicrtk[1],qlp.bicrtk[3]]) $
			else s3=s2
		end
		'slit': s3 = s0[*,*,0:3]
		endcase
	endelse


	if keyword_set(show) then begin
		imgsize, s3, nx, ny, nn
		wbin=1
		if (nx gt 512) or (ny gt 512) then wbin=2
		if (nx gt 1024) or (ny gt 1024) then wbin=4
		if keyword_set(winit) then begin
			s3[*,*,1:3]=0
			ku_dispiquvr,s3,bin=wbin,pmax=qlp.pmax,wid=wid
		endif else begin
			for i=1,3 do s3[*,*,i]=s3[*,*,i]/s3[*,*,0]
			ku_dispiquvr,s3,bin=wbin,pmax=qlp.pmax,wid=wid,/wexist
		endelse
		
	endif
	return,s3
end
;********************************************************************
pro qlpol_setdark, img3d, expo
	common qlpol_common, qlp, wdqlp, wid
	common qlpol_data_commom, s0, mm, date_obs0, very_dark, dark, dark_expo
	
	print, '[qlpol] adding binning to large format camera (orca) -> disable dark'
	return
	
	dark_expo=expo
	ss=size(img3d)
	case ss[0] of
		2 : img2d=img3d
		3 : img2d = reform(rebin(img3d,ss[1],ss[2],1))
		else: begin
			print, '[qlpol] ERROR: dark is ', ss[0], ' dimensional array'
		end
	endcase
	
	dark = img2d[qlp.box[0]:qlp.box[2],qlp.box[1]:qlp.box[3]]
	print,'[qlpol] set dart with expo=',dark_expo
end
;********************************************************************
pro qlpol_getpar, is_realtime=is_realtime, wave0=wave0, incli=incli, pmax=pmax, alive=alive
	common qlpol_common, qlp, wdqlp
	
	
	if not keyword_set(qlp) then begin
		alive=0
		is_realtime=0
		wave0=10830.
		incli=0.
		pmax=0.02
	endif else begin
		alive=qlp.alive
		is_realtime=qlp.is_realtime
		wave0=qlp.wave0
		incli=qlp.incli
		pmax=qlp.pmax
	endelse
end
;********************************************************************
pro qlpol_update_mmpar, incli=incli, wave0=wave0, telpos=telpos 
	common qlpol_common, qlp, wdqlp

	if keyword_set(incli) then qlp.incli = incli
	if keyword_set(wave0) then qlp.wave0 = wave0
	if keyword_set(telpos) then qlp.telpos = telpos

	if keyword_set(wave0) or keyword_set(telpos) then qlp.par = par_dst(qlp.wave0,qlp.telpos)

end

;********************************************************************
pro qlpol_update_obspar, expo=expo, rotp=rotp, alive=alive
	common qlpol_common, qlp, wdqlp

	if keyword_set(expo) then qlp.expo = expo
	if keyword_set(rotp) then qlp.rotp = rotp
	if keyword_set(alive) then qlp.alive = alive
end

;********************************************************************
pro qlpol_event, ev
    common qlpol_common, qlp, wdqlp
	common qlpol_data_commom, s0, mm, date_obs0, very_dark, dark

    widget_control, ev.id, get_uvalue=value

    case (ev.id) of
	 wdqlp.PMAX : begin
		qlp.pmax=float(gt_wdtxt(ev.id))
		print, '[qlpol] pmax = ', qlp.pmax
		dmy=qlpol_stokes(udf, udf, udf, udf, udf,/show,/replot)
	 end
	 
	 wdqlp.WAVELENGTH : begin
		wave0=float(gt_wdtxt(ev.id))
		qlpol_update_mmpar,wave0=wave0
		print, '[qlpol] wave0 = ', qlp.wave0
	 end

	 wdqlp.WL_ORDER : begin
		widget_control, ev.id, get_value=value
		qlp.wl_order=value
		print, '[qlpol] wl_order = ', qlp.wl_order
	 end

	 wdqlp.IS_REALTIME : begin
		widget_control, ev.id, get_value=value
		qlp.IS_REALTIME=value
		print, '[qlpol] is_realtime = ', qlp.is_realtime
	 end

	 wdqlp.PRINT_MM : begin
		widget_control, ev.id, get_value=value
		qlp.print_mm=value
		print, '[qlpol] print_mm = ', qlp.print_mm
	 end

	 wdqlp.STOKES : begin
		widget_control, ev.id, get_value=value
		case value of
			0: qlp.stokes = 'sun'
			1: qlp.stokes = 'slit'
		endcase
		print, '[qlpol] stokes = ', qlp.stokes
		dmy=qlpol_stokes(udf, udf, udf, udf, udf,/show,/replot)
	 end

	 wdqlp.TELPOS : begin
		widget_control, ev.id, get_value=value
		case value of
			0: qlp.telpos = 'WEST'
			1: qlp.telpos = 'EAST'
		endcase
		print, '[qlpol] telpos = ', qlp.telpos
		dmy=qlpol_stokes(udf, udf, udf, udf, udf,/show,/replot)
	 end

	 wdqlp.INCLID : begin
		inclid=float(gt_wdtxt(wdqlp.INCLID))
		inclim=float(gt_wdtxt(wdqlp.INCLIM))
		incli = inclid + inclim/60.
		qlpol_update_mmpar,incli=incli
		print, '[qlpol] incli = ', qlp.incli
		dmy=qlpol_stokes(udf, udf, udf, udf, udf,/show,/replot)
	 end

	 wdqlp.INCLIM : begin
		inclid=float(gt_wdtxt(wdqlp.INCLID))
		inclim=float(gt_wdtxt(wdqlp.INCLIM))
		incli = inclid + inclim/60.
		qlpol_update_mmpar,incli=incli
		print, '[qlpol] incli = ', qlp.incli
		dmy=qlpol_stokes(udf, udf, udf, udf, udf,/show,/replot)
	 end

	 wdqlp.EXIT : begin
		qlp.alive=0
		WIDGET_CONTROL, /destroy, ev.top
	 end
	
    
    else: 
    
    endcase
end

;********************************************************************
function qlpol_widget, base, qlp
    wd={wd_qlp_v1, $
;;        SELECT_AREA : 0L, $
;;        IS_REALTIME0: 0L, $
        IS_REALTIME : 0L, $
		PRINT_MM    : 0L, $
		STOKES      : 0L, $
        WAVELENGTH  : 0L, $
        WL_ORDER    : 0L, $
        PMAX        : 0L, $
        TELPOS      : 0L, $
        INCLID      : 0L, $
        INCLIM      : 0L, $
		EXIT        : 0L  $
    }

    b1=widget_base(base, /column, /frame)

;;    b11=widget_base(b1, /row)
;;    wd.SELECT_AREA=widget_button(b11, value="SELECT_AREA", uvalue = "SELECT_AREA")

    b13=widget_base(b1, /row)
    dmy = widget_label(b13, value='wave0: ')
    wd.WAVELENGTH = widget_text(b13,value=string(qlp.wave0,form='(i5)'), xsize=6, uvalue='WAVELENGTH',/edit)
    dmy = widget_label(b13, value='pmax: ')
    wd.PMAX = widget_text(b13,value=string(qlp.pmax,form='(f5.3)'), xsize=6, uvalue='PMAX',/edit)

    b17=widget_base(b1, /row)
    dmy = widget_label(b17, value='incli: ')
    wd.INCLID = widget_text(b17,value=string(0,form='(i3)'), xsize=4, uvalue='INCLID',/edit)
    dmy = widget_label(b17, value='D ')
    wd.INCLIM = widget_text(b17,value=string(0,form='(i3)'), xsize=4, uvalue='INCLIM',/edit)
    dmy = widget_label(b17, value='M')


    b12=widget_base(b1, /row, sensitive=1)
    dmy = widget_label(b12, value='Realtime:  ')
    wd.IS_REALTIME  = cw_bgroup(b12,['NO','YES'],/row, $
        uvalue="IS_REALTIME",/no_release,set_value=qlp.is_realtime,/exclusive,ysize=25,/frame,xsize=125)
;;    wd.IS_REALTIME0=b12

	b19=widget_base(b1, /row, sensitive=1)
    dmy = widget_label(b19, value='Print MM:  ')
    wd.PRINT_MM  = cw_bgroup(b19,['NO','YES'],/row, $
        uvalue="PRINT_MM",/no_release,set_value=qlp.print_mm,/exclusive,ysize=25,/frame,xsize=125)

	b18=widget_base(b1, /row, sensitive=1)
    dmy = widget_label(b18, value='Stokes:    ')
    wd.STOKES  = cw_bgroup(b18,['sun','slit'],/row, $
        uvalue="STOKES",/no_release,set_value=0,/exclusive,ysize=25,/frame,xsize=125)

    b14=widget_base(b1, /row)
    dmy = widget_label(b14, value='telpos:    ')
    wd.TELPOS  = cw_bgroup(b14,['WEST','EAST'],/row, $
        uvalue="TELPOS",/no_release,set_value=0,/exclusive,ysize=25,/frame,xsize=125)

    b15=widget_base(b1, /row)
    dmy = widget_label(b15, value='Blue:      ')
    wd.WL_ORDER  = cw_bgroup(b15,['Bot','Top'],/row, $
        uvalue="wl order",/no_release,set_value=qlp.wl_order,/exclusive,ysize=25,/frame,xsize=125)

    b16=widget_base(b1, /row,/Align_right)
    wd.Exit=widget_button(b16, value="Exit", uvalue = "Exit")


    return, wd

end
;********************************************************************
pro qlpol_main, img3d, date_obs, camera, expo, rotp
;; usage
;; IDL> qlpol_main,fltarr(640,512,10),'2021-10-10T09:41:57.911','GOLDEYE',0.015,1.0
	common qlpol_common, qlp, wdqlp, wid
	common qlpol_data_commom, s0, mm, date_obs0, very_dark, dark, dark_expo
	
    ;img3d = uintarr(2048,2048,10)
	;date_obs='2021-10-10T09:41:57.911'

	ss=size(img3d)
	if ss[0] ne 3 then begin 
		print, '[qlpol] must input 3 dimensional image'
		return
	endif
	if ss[3] le 1 then begin
		print, '[qlpol] must have nimg>1'
		return
	endif
	nx=ss[1] & ny=ss[2] & nn=ss[3]
	color='d67e40'x
	bin=1
	if (nx gt 1024) or (ny gt 1024) then bin=2
	wnx=nx/bin & wny=ny/bin
	wid=21
	window, wid, xs=wnx, ys=wny
	if bin gt 1 then tvscl, rebin(img3d[*,*,0],wnx,wny,1) $
	else tvscl, img3d[*,*,0]
	wait, 0.2
	print, '[qlpol] select box for IQUV demodulation'
	box_cur1, x0b, y0b, nxb, nyb,color=color
	qlp = qlpol_st()
	qlp.box[0]=x0b & qlp.box[1]=y0b & qlp.box[2]=x0b+nxb & qlp.box[3]=y0b+nyb
	qlp.box[*] = qlp.box[*]/2 ;; force even number
	qlp.box[*] = qlp.box[*]*2 
	qlp.box[*] *= bin
	print, '[qlpol] selected : (', qlp.box[0], ',',qlp.box[1],'), (',qlp.box[2],',',qlp.box[3],')'
	print, '[qlpol] select box for I2QUV correction'
	box_cur1, x0b, y0b, nxb, nyb,color=color
	qlp.bicrtk[0]=x0b & qlp.bicrtk[1]=y0b & qlp.bicrtk[2]=x0b+nxb & qlp.bicrtk[3]=y0b+nyb
	qlp.bicrtk[*] = qlp.bicrtk[*]/2 ;; force even number
	qlp.bicrtk[*] = qlp.bicrtk[*]*2
	qlp.bicrtk[*] *= bin
	for i=0,1 do $
	if qlp.bicrtk[i] lt qlp.box[i] then qlp.bicrtk[i]=0 $ 
	else qlp.bicrtk[i]=qlp.bicrtk[i]-qlp.box[i]

	for i=2,3 do $
	if qlp.bicrtk[i] gt qlp.box[i] then qlp.bicrtk[i]=qlp.box[i]-qlp.box[i-2] $ 
	else qlp.bicrtk[i]=qlp.bicrtk[i]-qlp.box[i-2]
	print, '[qlpol] selected (',qlp.bicrtk[0],',',qlp.bicrtk[1],'), (',qlp.bicrtk[2],',',qlp.bicrtk[3],')'
	wait, 0.3
	wdelete, wid

	s = qlpol_stokes(img3d, date_obs, camera, expo, rotp, bin=1, /show, /winit)

	base = WIDGET_BASE(title='DSTPOL PREVIEW', /column,xoffset=600, yoffset=300)
	wdqlp = qlpol_widget(base, qlp)
	widget_control, base, /realize
	XMANAGER, 'QLPOL', base, /NO_BLOCK
	qlp.alive=1
END
