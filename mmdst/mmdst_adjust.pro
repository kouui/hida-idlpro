;  mmdst_adjust.pro
;  2022.01.24	u.k.	  mmdst_manual_adjust
;  2022.01.25	k.i.	  Auto, Disp
;  2022.01.29      u.k.    Q/U/V symmetry fit
;  2022.02.04      u.k.    conf_symfit, added th_vs to widget
;  2022.02.06      u.k.    added th_vs to symfit
;  2022.02.06      u.k.    added fix_th_vs. fix_sc to widget, symfit
;  2022.02.07      u.k.    pro mmdst_adjust; *.xml configuration file
;  2022.02.10      u.k.    *.xml -> *.conf
;  2022.02.11      u.k.    corrected order of array dimension
;------------------------------------------------------------------------
;; external dependency
@mmdst
@dst_pollib
@mmdst_lib
@quv_symfit
@mmdsttxt


;------------------------------------------------------------------------
;; UPDATE PROFILES
;; s0 (1, npix_y, 4), 4 : I,Q,U,V
;FUNCTION update_profiles, s0, mm
;  
;   rmm = invert(mm)
;   ss = size(s0)
;   ny = ss[2]
;   s3 = fltarr(1,ny,ss[3])
;   for j=0,3 do begin
;		s3[*,*,j] = rmm[0,j]*s0[*,*,0]		
;             for i=1,3 do s3[*,*,j] = s3[*,*,j] + rmm[i,j]*s0[*,*,i]
;	endfor
;   return, s3
;END

;------------------------------------------------------------------------
;; UPDATE PROFILE PLOTS
;; profs (1, npix_y, 4)
PRO update_profile_plots, wl, profs, wid, init=init, s0=s0, s2=s2
   
   if keyword_set(init) then begin 
      window, wid, xs=1200,ys=850
   endif else begin
      wset, wid
   endelse
	
   color_fix = 'ffc466'x
   ;iprof = reform(profs[0,*,0])
   ;qprof = reform(profs[0,*,1])
   ;uprof = reform(profs[0,*,2])
   ;vprof = reform(profs[0,*,3])
   labels = ['  I','Q/Ic','U/Ic','V/Ic']

   blank=replicate(' ',10)
   wy1=0.22
	box=[0.1,0.,.95,wy1]
	yoff=[0,1,0,1]
   cs=2.2
   for i=0,3 do begin
      prof = reform(profs[0,*,i])
      if i lt 3 then xtickname=blank else UNDEFINE, xtickname
      if i eq 0 then begin
          yr = [0,max(prof)]
          ;;ytext = 1
          plot,wl,prof,pos=box+yoff*(0.1+wy1*(3-i)),/norm,xtickname=xtickname, chars=cs,xstyle=1,yr=yr,ystyle=1
          ;;kk = 460
          ;;oplot,[wl[kk],wl[kk]], [0,0.8],color='ffffff'x
          ;;kk = 445
          ;;oplot,[wl[kk],wl[kk]], [0,0.8],color='ffffff'x
          ;;kk = 110
          ;;oplot,[wl[kk],wl[kk]], [0,0.8],color='ffffff'x
      endif else begin 
          ;;yr = 0.1*[-1,1]
          yabsmax = max(abs(prof))
          yr = yabsmax*[-1,1]
          ;;ytext = yr[1]*0.6
          plot,wl,prof,pos=box+yoff*(0.1+wy1*(3-i)),/norm,xtickname=xtickname, chars=cs,xstyle=1,yr=yr,ystyle=1,/noerase
          oplot,[wl[0],wl[-1]],[0,0],linestyle=1
      endelse
      if keyword_set(s0) then oplot, wl, s0[0,*,i], color=color_fix, linestyle=2
      if keyword_set(s2) and (not keyword_set(init)) then oplot, wl, s2[0,*,i], color=color_fix
      ytext = yr[1]*0.6
      xyouts,wl[0],ytext,labels[i],/data,chars=2.5
   endfor
   

   
END

;------------------------------------------------------------------------
;; RETURN CURSOR CLICK COORDINATE
FUNCTION click_event, wid=wid, msg=msg, data=data
    if keyword_set(wid) then wset, wid
    if keyword_set(msg) then print, msg
    
    if keyword_set(data) then cursor,xpos,ypos,/data,/down $
    else cursor,xpos,ypos,/dev,/down
    return, [xpos, ypos]
END
;------------------------------------------------------------------------
;; SELECT X POS WITH CURSOR
;; s4 (npix_x, npix_y, 4), 4 : I,Q,U,V
FUNCTION select_xpos, s4, pmax=pmax, bin=bin, wid=wid, ialog=ialog, islabel=islabel

   if not keyword_set(wid) then wid=0
   if not keyword_set(pmax) then pmax=0.05
   if not keyword_set(bin) then bin=1
   coms = ['I', 'Q', 'U', 'V']

	ss = size(s4)
   if not ss[0] eq 3 then throw_error, "ndim of s4 !=3"
   if not ss[3] eq 4 then throw_error, "size of the 3rd dimension of s4 !=4"
   nx0 = ss[1]
   ny0 = ss[2]
   ns0 = ss[3]
   nx = nx0 / bin
   ny = ny0 / bin
   
   dd = 2
   window,wid,xs=nx*ns0+dd*(ns0-1),ys=ny+dd*2
   ;; show I spectrum image
   x0 = 0
   x0s = [x0,0,0,0]
   y0 = 0
   i2d=congrid(s4[*,*,0],nx,ny)
   if keyword_set(ialog) then tvscls,alog(i2d),x0,y0+dd $
   else tvscls,i2d,x0,y0+dd,sgm=[-3,1]
   if keyword_set(islabel) then xyouts,x0+10,ny-30,coms[0],chars=3,charthick=2,/dev
   right_bounds = [x0 + nx,0,0,0]
   ;; show Q, U, V spectrum image
   for j=1,ns0-1 do begin
		x0 = x0 + nx + dd
		tvscls,congrid(s4[*,*,j],nx,ny),x0,y0+dd,vmin=-pmax,vmax=pmax
		if keyword_set(islabel) then xyouts,x0+10,ny-30,coms[j],chars=3,charthick=2,/dev
	   right_bounds[j] = x0 + nx
      x0s[j] = x0
   endfor

   ;; click and compute x position
   pos = click_event(msg="click left button of the mouse to select a x position ...")
	xpos = pos[0]

   bias = 0
   for j=0,ns0-1 do begin
      if right_bounds[j] gt xpos then break
      bias = bias + (nx+dd)
   endfor

   xpos = xpos - bias
   
;;   print, j, xpos
   for j=0, ns0-1 do begin
      draw, (x0s[j]+xpos)*[1,1], [dd,dd+ny], color=150, thick=2
   endfor
   xpos = xpos*bin
   print, 'select xpos=', xpos
   return, xpos
END

;------------------------------------------------------------------------
;[bug not yet fixed]
function sfunc_mmdst,cx,x
   common mmadjust, pars, wds, wd, sdata, pdata

	mm = update_mmdst(dst, telpos, x[0], x[1], x[2], x[3], x[4])
      s11 = UPDATE_S3(s0, mm) 
   	s1 = UPDATE_S3(s4[*,80:180,*], mm)
	update_profile_plots, wl, s11, 12, s0=s0, s2=s2

	;s1 = s1[*,600:900,*]
	;s1[*,*,2] = -s1[*,*,2]
	ccIQ = total(s1[*,*,0]*s1[*,*,1])
	ccIU = total(s1[*,*,0]*s1[*,*,2])
	ccIV = total(s1[*,*,0]*s1[*,*,3])
	ccVQ = total(s1[*,*,3]*s1[*,*,1])
	ccVU = total(s1[*,*,3]*s1[*,*,2])
	ccc = [ccIQ, ccIU, ccIV, ccVQ, ccVU]
	return,ccc

end


;------------------------------------------------------------------------
PRO mmdst_event, ev
   common mmadjust, pars, wds, wd, sdata, pdata
   common config, conf
   common wconfig, wid_profile

   conf_symfit = conf.sconf
   bin = conf.bin

	widget_control, ev.id, get_uvalue=value

   	case (ev.id) of
      wds.fix_th_vs: conf.sconf.fix_th_vs =  widget_info(wds.fix_th_vs,/button_set)
      wds.fix_sc: conf.sconf.fix_sc =  widget_info(wds.fix_sc,/button_set)
      wds.th_vs : pars.th_vs = gt_wdtxt(ev.id)
		wds.sc : pars.sc = gt_wdtxt(ev.id)
		wds.xn : pars.xn = gt_wdtxt(ev.id)
		wds.tn : pars.tn = gt_wdtxt(ev.id)
		wds.xc : pars.xc = gt_wdtxt(ev.id)
		wds.tc : pars.tc = gt_wdtxt(ev.id)
		wd.Auto: begin
			;  I-Q, I-U, I-V, V-Q, V-U
			cx = [1,1,1,1,1]
			target = [0,0,0,0,0]
			weight = [1,1,1,20,20]
			eps = 1.e-4
			niter = 200
			quiet = 0
			err = [eps,eps,eps,eps^2,eps^2]*sqrt(n_elements(s4[*,80:180,0]))
			xini = [pars.xn, pars.tn, pars.xc, pars.tc, pars.sc]
                   ;;xini = [0,0,0,0,0]
                   parinfo = make_parinfo(pars.xn, pars.tn, pars.xc, pars.tc, pars.sc)
                      print, parinfo
			x = mpfitfun('sfunc_mmdst',cx,target,err,xini,weight=weight,errmsg=errmsg, $
				quiet=quiet,maxiter=niter,niter=k,yfit=sfit,bestnorm=eval1,parinfo=parinfo);,functarg=l)
			;	x = dls(xini,'sfunc_mmdst',target,weight=weight,parms=parms, $
			;	niter=niter,damp=damp,rofct=rofct,/verbose,eval=eval,/pause,symsize=sms)
			pars.xn=x[0] & 	pars.tn=x[1] &	pars.xc=x[2] &	pars.tc=x[3] &	pars.sc=x[4]
			widget_control, wds.xn,set_value=pars.xn
			widget_control, wds.tn,set_value=pars.tn
			widget_control, wds.xc,set_value=pars.xc
			widget_control, wds.tc,set_value=pars.tc
			widget_control, wds.sc,set_value=pars.sc
			print,'CC=',ccc
			return
      end
	   wd.SFit: begin    
         parinit = [pars.xn, pars.tn, pars.xc, pars.tc, pars.sc, pars.th_vs]
			;;parinit = [0.00300, -0.06286, 0.02500, 0.12566, 0.0]
         parinfo = MAKE_PARINFO(parinit[0], parinit[1], parinit[2], parinit[3], parinit[4], parinit[5], conf_symfit)
         err = conf_symfit.error
         niter = conf_symfit.niter
         weight = {weight_struct, wtQ:2.0, wtU:2.0, wtV:1.0, wtL:1.0, wtC:1.0}
         pars_fit = MPFIT_PARDST(conf_symfit, sdata.s4d[conf_symfit.islit1:conf_symfit.islit2,*,*,*], sdata.dst, parinit, parinfo, err, weight, niter=niter)
			;;pars_fit = TEST_MPFIT_PARDST(s4, dst)
			pars.xn = pars_fit[0]
			pars.tn = pars_fit[1]
			pars.xc = pars_fit[2]
			pars.tc = pars_fit[3]
			pars.sc = pars_fit[4]
         pars.th_vs = pars_fit[5]
         widget_control, wds.xn,set_value=pars.xn
			widget_control, wds.tn,set_value=pars.tn
			widget_control, wds.xc,set_value=pars.xc
			widget_control, wds.tc,set_value=pars.tc
			widget_control, wds.sc,set_value=pars.sc
         widget_control, wds.th_vs,set_value=pars.th_vs
	   end
		wd.Disp: begin
         nf = n_elements(sdata.dst)
         step = nf/2
         for i=0,nf-1 do begin
            if (i mod step) ne 0 then continue
            mm = update_mmdst(sdata.dst[i], pars.xn, pars.tn, pars.xc, pars.tc, pars.sc, th_vs=pars.th_vs)
            sr = UPDATE_S3(sdata.s4d[*,*,*,i],mm)
            if conf_symfit.corr_Icrtk then begin
               yr = [conf_symfit.iobs1,conf_symfit.iobs2]
               sr = correct_Icrtk(sr,get_coeffs=get_coeffs,yr=yr)
            endif
            dispiquvr,sr,bin=bin,pmax=0.03,/ialog
         endfor
         return
      end
		wd.Exit: begin
			WIDGET_CONTROL, /destroy, ev.top
		end
      wd.Icrtk: begin
			val = widget_info(wd.Icrtk,/button_set)
         conf.sconf.corr_Icrtk = val
		end
   	endcase
	
	mm = update_mmdst(sdata.dst[0], pars.xn, pars.tn, pars.xc, pars.tc, pars.sc, th_vs=pars.th_vs)
   s1 = UPDATE_S3(pdata.s2d_origin, mm)
	update_profile_plots, sdata.wl, s1, wid_profile, s0=pdata.s2d_origin, s2=pdata.s2d_anan
END
;------------------------------------------------------------------------
;; widget layout for sliders
FUNCTION slider_widget,base, pars, conf_symfit
  ;--------------------------------------------------------------
  wd={wd_slider, $
    th_vs:0l, $
    fix_th_vs:0l, $
    fix_sc:0l, $
    sc:   0l, $
    xn:   0l, $
    tn:   0l, $
    xc:   0l, $
    tc:   0l $
 	}

   dmax = 0.2	; diattenuation
   rmax = 90.	; retardation, deg.
	
   b1=widget_base(base, /column, /frame)
   dmy = widget_label(b1, value='>>> ... <<<')
   
   ;; th_vs
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='th_vs=')
   wd.th_vs = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+45.0,minimum=-45.0, value=pars.th_vs)
   dmy=widget_base(b_row, /row, /NonExclusive)
   wd.fix_th_vs = Widget_Button(dmy, Value='fixed')
   Widget_Control, wd.fix_th_vs, Set_Button=conf_symfit.fix_th_vs

   ;; sc
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='sc=')
   wd.SC = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+0.3,minimum=-0.3, value=pars.sc)
   dmy=widget_base(b_row, /row, /NonExclusive)
   wd.fix_sc = Widget_Button(dmy, Value='fixed')
   Widget_Control, wd.fix_sc, Set_Button=conf_symfit.fix_sc

   ;; xn
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='xn=')
   wd.XN = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+dmax,minimum=-dmax, value=pars.xn)

   ;; tn
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='tn=')
   wd.TN = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+rmax*!dtor,minimum=-rmax*!dtor, value=pars.tn)

   ;; xc
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='xc=')
   wd.XC = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+dmax,minimum=-dmax, value=pars.xc)

   ;; tn
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='tc=')
   wd.TC = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+rmax*!dtor,minimum=-rmax*!dtor, value=pars.tc)
	return, wd
END
;------------------------------------------------------------------------
;; MAIN PRCEDURE

;; trial@2021.01.23 10830
;; xpos = 
;; sc = 0.0
;; xn = -0.04200
;; tn = -0.24436
;; xc = -0.02800
;; tc = -0.10474
;; xpos = 135 (fix sc,th_vs)
;; th_vs = 0.0
;; sc = 0.0
;; xn = -0.04015
;; tn = -0.23124
;; xc = -0.02723
;; tc = -0.08398
;; xpos = 135 (fix sc)
;; th_vs = 10.97757
;; sc = 0.0
;; xn = -0.03461
;; tn = -0.17302
;; xc = -0.01423
;; tc = +0.00101

FUNCTION mmdst_adjust, s, dst, wl0, bin=bin, sconf=sconf, path=path
;PRO mmdst_adjust, path, 

common mmadjust, pars, wds, wd, sdata, pdata
common config, conf
common wconfig, wid_profile

;DELETE_VARIABLE = 1b


;;-----------------------------------------------------
;; variables needed to be modified manually

;path = '/tmp_mnt/home/kouui/idlpro/mmdst/He1083.20220110.mmdst_adjust.conf'
;UNDEFINE,path
;;-----------------------------------------------------
;; read configuration variables from txt
conf = MMDST_ADJUST_TXT_READ(path)

if keyword_set(path) then begin

bin = conf.bin
wl0 = conf.wl0
conf_symfit = conf.sconf
xpos = conf.xpos;  xpos to extract 1D IQUV profiles. if eq -1, then select xpos with cursor click
savfolder = conf.SAVFOLDER

;;-----------------------------------------------------

;; read .sav files from savfolder
;; in the *.sav file :
;; s, (nxp,nyp,>4), iquv[r...] after demodulation
;; dst, (3,11), dst status

;; clear variables
UNDEFINE,s & UNDEFINE,dst

savfiles = findfile(savfolder + '/*.sav')
nf = n_elements(savfiles)
if nf eq 0 then throw_error, '0 .sav files found in ' + savfolder
print, "use ", nf, " .sav files for symfit"

restore, savfiles[0]; read s, dst, wl, telpos
ss = size(s)
if not ss[0] eq 3 then throw_error, 's should be a 3d array'
nx = ss[1] & ny = ss[2] & ns = 4
sseq   = reform(fltarr(nx,ny,ns,nf),nx,ny,ns,nf)
sseq[*,*,*,0] = s[*,*,0:3]
dstseq = [dst]

for i=1,nf-1 do begin
   restore, savfiles[i]; read s, dst, wl, telpos
   ss = size(s)
   if not ss[0] eq 3 then throw_error, 's should be a 3d array, savfile= '+savfiles[i]
   sseq[*,*,*,i] = s[*,*,0:3]
   dstseq = [dstseq,[dst]]
endfor

endif else begin
ss = size(s)
if ss[0] ne 3 then throw_error, 's has ndim != 3'
conf.wl0 = wl0
if keyword_set(bin) then conf.bin=bin else conf.bin=1
sseq = reform(s, ss[1],ss[2],ss[3],1)
dstseq = [dst]
endelse
ss = size(sseq)
if not keyword_set(wl) then wl = indgen(ss[2])

sdata = {symfit_data, $ ; 
s4d    : sseq, $        ; fltarr, (nx,ny,ns,nf)
dst    : dstseq, $      ; uintarr, (3,11nf)
wl     : wl $          ; wavelength array
}
UNDEFINE,s & UNDEFINE,dst & UNDEFINE,wl & UNDEFINE,ss
UNDEFINE,sseq & UNDEFINE,dstseq

;restore, savfile
;;-----------------------------------------------------
;if keyword_set(sconf) then conf.sconf = sconf
;sconf = conf.sconf
;if (sconf.iedgeL eq 0) or (sconf.icentL eq 0) then 


;;-----------------------------------------------------

;s4 = s[*,*,0:3] ;; iquv
s3d = sdata.s4d[*,*,*,0]
if conf.xpos eq -1 then conf.xpos = select_xpos(s3d, pmax=0.01, wid=11, bin=bin, /islabel)
s2d = s3d[conf.xpos,*,*] ;; iquv at a single x position

;; initialize
pars_anan = par_dst(wl0, sdata.dst[0].pos)
;;print,pars_anan

pars = {mmdst_pars, th_vs:0.0, sc:0.0, xn:pars_anan.xn, tn:pars_anan.tn, xc:pars_anan.xc, tc:pars_anan.tc}
pars_init = pars

mm = update_mmdst(sdata.dst[0], pars.xn, pars.tn, pars.xc, pars.tc, pars.sc, th_vs=pars.th_vs)
s2d_anan = UPDATE_S3(s2d, mm)

pdata = {plot_data, s2d_origin:s2d, s2d_anan:s2d_anan}

wid_profile = 12
update_profile_plots,sdata.wl,s2d_anan,wid_profile,s0=s2d,s2=s2d_anan,/init
;stop

UNDEFINE, mm
UNDEFINE, s3d
UNDEFINE, s2d
UNDEFINE, s2d_anan



base = WIDGET_BASE(title='MMDST_ADJUST', /column)
wds = slider_widget(base, pars, conf.sconf)
wd = {main_wd, $
   Icrtk: 0l, $
	Auto:  0l, $	;
   SFit:  0l, $ ;
	Disp:	 0l, $	;
	Exit:	 0l $
	}
b1 = widget_base(base, /row)
dmy=widget_base(b1, /row, /NonExclusive)
wd.Icrtk = Widget_Button(dmy, Value='CorrIcrtk')
Widget_Control, wd.Icrtk, Set_Button=conf.sconf.corr_Icrtk
wd.Auto = widget_button(b1, value="Auto", uvalue = "Auto", SENSITIVE = 0)
wd.SFit = widget_button(b1, value="SFit", uvalue = "SFit")
wd.Disp = widget_button(b1, value="Disp", uvalue = "Disp")
wd.Exit = widget_button(b1, value="Exit", uvalue = "Exit")

widget_control, base, /realize
;;ret = SELECT_PROFILE_RANGE(wid_profile,wl)
XMANAGER, 'mmdst', base

return, {mmdst_adjust_struct, conf:conf, pars:pars, pars_init:pars_init}

END




file = '/nwork/kouui/data-lvl1/dstpol/20220110.giant-prominence/spec/cal/sav.for-mmdst-adjust'
file = file + '/step5.s.ar.sav'
restore, file
wl0 = 10830
pcal = mmdst_adjust(s[*,*,0:3], dst, wl0, bin=1)

END
