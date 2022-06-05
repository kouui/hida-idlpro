
@lib.dstpol.mmdst
@lib.dstpol.mmdst_lib

;------------------------------------------------------------------------
;; widget layout for sliders
FUNCTION i2quv_widgets, base, pars
  ;--------------------------------------------------------------
  wd={wd_slider, $
    istray:0l, $
 ;   th_vs:0l, $
    fix_th_vs:0l, $
    fix_sc:0l, $
    sc:   0l, $
    xn:   0l, $
    tn:   0l, $
    xc:   0l, $
    tc:   0l, $
    Icrtk: 0l, $
    Reset: 0l, $
    Disp: 0l, $
    Exit: 0l $
 	}

   dmax = 0.2	; diattenuation
   rmax = 90.	; retardation, deg.
	
   b1=widget_base(base, /column, /frame)
   dmy = widget_label(b1, value='>>> I2QUV(t) <<<')
   
   ;; i stray offset
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='st=')
   wd.istray = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+0.3,minimum=0.0, value=pars.istray, scroll=0.001)


   ;; th_vs
  ;  b_row=widget_base(b1, /row)
  ;  dmy = widget_label(b_row, value='th_vs=')
  ;  wd.th_vs = CW_FSLIDER(b_row,format='(f8.2)',/edit,/drag,maximum=+45.0,minimum=-45.0, value=pars.th_vs)
  ;  dmy=widget_base(b_row, /row, /NonExclusive)
  ;  wd.fix_th_vs = Widget_Button(dmy, Value='fixed')
  ;  Widget_Control, wd.fix_th_vs, Set_Button=0b

   ;; sc
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='sc=')
   wd.SC = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+0.3,minimum=-0.3, value=pars.sc,scroll=0.1)
   ;dmy=widget_base(b_row, /row, /NonExclusive)
   ;wd.fix_sc = Widget_Button(dmy, Value='fixed')
   ;Widget_Control, wd.fix_sc, Set_Button=1b

   ;; xn
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='xn=')
   wd.XN = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+dmax,minimum=-dmax, value=pars.xn,scroll=0.001)

   ;; tn
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='tn=')
   wd.TN = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+rmax*!dtor,minimum=-rmax*!dtor, value=pars.tn,scroll=!pi/180.*0.1)

   ;; xc
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='xc=')
   wd.XC = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+dmax,minimum=-dmax, value=pars.xc,scroll=0.001)

   ;; tn
   b_row=widget_base(b1, /row)
   dmy = widget_label(b_row, value='tc=')
   wd.TC = CW_FSLIDER(b_row,format='(f8.5)',/edit,/drag,maximum=+rmax*!dtor,minimum=-rmax*!dtor, value=pars.tc,scroll=!pi/180.*0.1)


   b_row = widget_base(base, /row)
   dmy=widget_base(b_row, /row, /NonExclusive)
   wd.Icrtk = Widget_Button(dmy, Value='CorrIcrtk')
   Widget_Control, wd.Icrtk, Set_Button=0b
   wd.Disp = widget_button(b_row, value="Disp", uvalue = "Disp")
   wd.Reset = widget_button(b_row, value="Reset", uvalue = "Reset")
   dmy = widget_label(b_row, value=' ')
   wd.Exit = widget_button(b_row, value="Exit", uvalue = "Exit")
   
   return, wd
END
;;-------------------------------------------------------------------
; pf (nf,4)
PRO update_pfplots, pfres, pforin, wid, init=init, pfanan=pfanan, wl=wl, overlap=overlap
@my_colors   
    if keyword_set(init) then window, wid, xs=1200,ys=850 $
    else wset, wid
    
    if not keyword_set(wl) then wl = indgen(n_elements(pfres[*,0]))

    if keyword_set(overlap) then begin

    yr = [0.5, 0.7]
    for i=0,3 do begin
      if i eq 0 then begin
        plot,wl,pfres[*,i],xtickname=xtickname,chars=cs,xstyle=1,yr=yr,ystyle=1,color=_colors[i]
        ;oplot,wl,pforin[*,i],color=_colors[i],linestyle=2
      endif else begin
        oplot,wl,pfres[*,i],color=_colors[i]
        ;oplot,wl,pforin[*,i],color=_colors[i],linestyle=2
      endelse

    endfor

    endif else begin

    color_fix = 'ffc466'x
    labels = ['  I','Q/Ic','U/Ic','V/Ic']
    
    blank=replicate(' ',10)
    wy1=0.22
    box=[0.1,0.,.95,wy1]
	  yoff=[0,1,0,1]
    cs=2.2

    ;;　 

    for i=0,3 do begin
      ;yr = [ min(pforin[*,i]),max(pforin[*,i])]
      yr = [1.,1.]*mean(pforin[*,i]) + 1.1*[-1.,+1.]*min( [abs(mean(pforin[*,i])-min(pforin[*,i])),abs(mean(pforin[*,i])-max(pforin[*,i]))] )
      if keyword_set(pfanan) then begin
        yr0 = yr
        yr = [1.,1.]*mean(pfanan[*,i]) + 1.1*[-1.,+1.]*min( [abs(mean(pfanan[*,i])-min(pfanan[*,i])),abs(mean(pfanan[*,i])-max(pfanan[*,i]))] )
        yr = [min([yr0[0],yr[0]]),max([yr0[1],yr[1]])]
      endif
      yr0 = yr
      yr = [1.,1.]*mean(pfres[*,i]) + 1.1*[-1.,+1.]*min( [abs(mean(pfres[*,i])-min(pfres[*,i])),abs(mean(pfres[*,i])-max(pfres[*,i]))] )
      yr = [min([yr0[0],yr[0]]),max([yr0[1],yr[1]])]
      if i lt 3 then xtickname=blank else UNDEFINE, xtickname
      if i eq 0 then begin
          plot,wl,pfres[*,i],pos=box+yoff*(0.1+wy1*(3-i)),/norm,xtickname=xtickname, chars=cs,xstyle=1,yr=yr,ystyle=1
          oplot,wl,pforin[*,i],color=color_fix, linestyle=2
      endif else begin 
          plot,wl,pfres[*,i],pos=box+yoff*(0.1+wy1*(3-i)),/norm,xtickname=xtickname, chars=cs,xstyle=1,yr=yr,ystyle=1,/noerase
          oplot,wl,pforin[*,i],color=color_fix, linestyle=2
          ;oplot,[wl[0],wl[n_elements(wl)-1]],[0,0],linestyle=1
      endelse
      if keyword_set(pfanan) and (not keyword_set(init)) then oplot,wl,pfanan[*,i],color=color_fix
      ;ytext = yr[1]-0.1
      ;xyouts,wl[0],ytext,labels[i],/data,chars=2.5
   endfor

   endelse
   
END

;;-------------------------------------------------------------------
FUNCTION update_pfs, pfs0, dsts, pars, ao=ao

pfs = pfs0
nf = n_elements(dsts)
for i=0,nf-1 do begin
  mm = UPDATE_MMDST(dsts[i], pars.xn, pars.tn, pars.xc, pars.tc, pars.sc, ao=ao)
  rmm = invert(mm)
  sm = fltarr(2,4) & s0 = fltarr(2,4)
  s0[0,*] = reform(pfs.atm[i,*])
  s0[1,*] = reform(pfs.conti[i,*])
  for j=0,3 do begin
    sm[*,j] = rmm[0,j]*s0[*,0]
    for jj=1,3 do sm[*,j] = sm[*,j] + rmm[jj,j]*s0[*,jj]
  endfor
  pfs.atm[i,*] = reform(sm[0,*])
  pfs.conti[i,*] = reform(sm[1,*])
endfor

return, pfs
END

;;-------------------------------------------------------------------
;; xmanager event handler
PRO event_handler, ev
  common widget_cblock, wds, wids
  common data_cblock, pfsorin, dsts, pars, ao, parsanan

  pfs = pfsorin
  
  widget_control, ev.id, get_uvalue=value
  case (ev.id) of

  wds.istray: begin
    pars.istray = gt_wdtxt(ev.id)
    parsanan.istray = pars.istray
  end

  wds.sc : pars.sc = gt_wdtxt(ev.id)
  wds.xn : pars.xn = gt_wdtxt(ev.id)
  wds.tn : pars.tn = gt_wdtxt(ev.id)
  wds.xc : pars.xc = gt_wdtxt(ev.id)
  wds.tc : pars.tc = gt_wdtxt(ev.id)

  wds.Reset: begin
    Widget_Control, wds.sc, set_value=parsanan.sc
    Widget_Control, wds.xn, set_value=parsanan.xn
    Widget_Control, wds.tn, set_value=parsanan.tn
    Widget_Control, wds.xc, set_value=parsanan.xc
    Widget_Control, wds.tc, set_value=parsanan.tc
    pars = parsanan
  end
  wds.Exit: begin
    WIDGET_CONTROL, /destroy, ev.top
    return
  end
  endcase

  pfs = pfsorin
  pfs.atm[*,0] = pfsorin.atm[*,0] - pars.istray
  pfs.conti[*,0] = pfsorin.conti[*,0] - pars.istray
  if ev.id eq wds.istray then begin
    update_pfplots, pfs.atm/pfs.conti, pfsorin.atm/pfsorin.conti, wids.div, /overlap
  endif

  pfsanan = update_pfs(pfs, dsts, parsanan, ao=ao)
  pfs = update_pfs(pfs, dsts, pars, ao=ao)
  update_pfplots, pfs.conti, pfsorin.conti, wids.conti, pfanan=pfsanan.conti
END

;;-------------------------------------------------------------------
;; main program
;; st = 0.057

common widget_cblock, wds, wids
common data_cblock, pfsorin, dsts, pars, ao, parsanan

;; atm (nf,4), float
;; conti (nf,4), float
;; dstarr (nf), dst_struct
;; files (nf), string

savf = "/nwork/kouui/data-lvl1/dstpol/20220517.i2quv-timeseries/atmconti.20220517.sav"
restore, savf
wl0 = 10830.
ao = mmao(wl0*0.1)
;pars_anan = par_dst(wl0, sdata.dst[0].pos)
pars_anan = par_dst(wl0, dstarr[0].pos)
pars = {mmdst_pars, istray:0.057, sc:0.0, xn:pars_anan.xn, tn:pars_anan.tn, xc:pars_anan.xc, tc:pars_anan.tc}
parsanan = pars

wids = {wid_struct, div:7, atm:8, conti:9}

pfs = {prof_struct, atm:atm, conti:conti}
dsts=dstarr
pfsorin = pfs
undefine, atm, conti, dstarr

pfs.atm[*,0] = pfsorin.atm[*,0] - pars.istray
pfs.conti[*,0] = pfsorin.conti[*,0] - pars.istray
update_pfplots, pfs.atm/pfs.conti, pfsorin.atm/pfsorin.conti, wids.div, /init, /overlap

pfsanan = update_pfs(pfs, dsts, parsanan, ao=ao)
pfs = pfsanan

update_pfplots, pfs.conti, pfsorin.conti, wids.conti, /init, pfanan=pfsanan.conti

base = WIDGET_BASE(title='I2QUV(t)', /column)
wds = i2quv_widgets(base, pars)
widget_control, base, /realize
XMANAGER, 'i2quvgui', base, EVENT_HANDLER='event_handler', /NO_BLOCK

END