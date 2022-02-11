; LIBRARY FOR FITTING SYMMETRY Q/U/V PROFILE OF SUNSPOT
;
; HISTORY : 
; 2021.01.27  U.K.  CREATED
; 2021.01.27  U.K.  almost completed, works in mmdst_adjust
; 2022.02.06      u.k.    added th_vs to symfit
; 2022.02.06      u.k.    added fix_th_vs. fix_sc to symfit
; 2022.02.11      u.k.    switch order of dimension in each array


; LIST :
; FUNCTION ARRAY1D_FIND_NEAREST, array, target, value=value
; FUNCTION CALCULATE_FIT_RANGE, xl_edge, xl_center, xc_edge, xc_center, sp, xdata=xdata, xl3=xl3, xc3=xc3, xls=xls, xcs=xcs
; FUNCTION INTERP_PROFILE, s4, xls, xcs, spil=spil, spic=spic
; FUNCTION MAKE_HALF_ARRAY, spil
; FUNCTION MAKE_ERROR_ARRAY, spil, err
; FUNCTION MAKE_WEIGHT_ARRAY, spil, wtQ=wtQ, wtU=wtU, wtV=wtV, wtL=wtL, wtC=wtC
; FUNCTION MAKE_RESULT_ARRAY, spil, spic, target=target, zero_netV = zero_netV
; FUNCTION INIT_PARINFO 
; FUNCTION MAKE_PARINFO, xn, tn, xc, tc, sc
; FUNCTION MODEL_PARDST, cx, par
; FUNCTION MPFIT_PARDST, s4, dst3, telpos1, parinit, parinfo, err, weight, niter=niter, quiet=quiet
; FUNCTION TEST_MPFIT_PARDST, s_origin, dst


;; EXTERNAL DEPENDENCY
@mmdst_lib

;------------------------------------------------------------------------
;; RETURN CURSOR CLICK COORDINATE
;; [don't use]
FUNCTION CLICK_EVENT, wid=wid, msg=msg, data=data
    if keyword_set(wid) then wset, wid
    if keyword_set(msg) then print, msg
    
    if keyword_set(data) then cursor,xpos,ypos,/data,/down $
    else cursor,xpos,ypos,/dev,/down
    return, [xpos, ypos]
END

;------------------------------------------------------------------------
;; SELECT RANGE OF OBSORPTION-LINE/CONTINUUM
;; [not yet implemented]
FUNCTION SELECT_PROFILE_RANGE, wid, xdata

    ss = size(xdata)
    if ss[0] ne 1 then throw_error, "ndim of xdata !=1"

    wset, wid
    plot, [0,1], [0,1]
    pos = click_event(msg="click left button of the mouse to select x-left of the obsorption line",/data)
    print, pos

    return, 0
END

;------------------------------------------------------------------------
;; FIND THE POSITION IN AN ARRAY 
;; WITH A VALUE MOSTLY CLOSE TO THE TARGET VALUE
FUNCTION ARRAY1D_FIND_NEAREST, array, target, value=value

    ss = size(array)
    if ss[0] ne 1 then throw_error, "ndim of array !=1"
    
    value = min( abs(array-target), pos )
    
    return, pos
END

;------------------------------------------------------------------------
;; CALCULATE THE RANGE OF OBSORPTION LINE AND CONTINUUM
;; - FOR OBSORPTION LINE : FIT THE MINIMUM AND INTERPOLATE THE GRID
;; - FOR CONTINUUM : INTERPOLATE TO HAVE THE SAME NUMBER OF POINTS WITH LINE
FUNCTION CALCULATE_FIT_RANGE, xl_edge, xl_center, xc_edge, xc_center, sp, xdata=xdata, xl3=xl3, xc3=xc3, xls=xls, xcs=xcs

    ;; if xl_edge, xl_center, xc_center is in unit of data
    ;; then convert them into unit of grid
    if keyword_set(xdata) then begin
        xl_edge = ARRAY1D_FIND_NEAREST(xdata,xl_edge)
        xl_center = ARRAY1D_FIND_NEAREST(xdata,xl_center)
        xc_center = ARRAY1D_FIND_NEAREST(xdata,xc_center)
    endif
    xl_edge   = fix(xl_edge)
    xl_center = fix(xl_center)
    xc_edge   = fix(xc_edge)
    xc_center = fix(xc_center)

    ;; fit minimum to find center
    x0 = x0fit(sp,xl_center,5)
    nx = fix( abs(x0-xl_edge) )
    nxl = nx
    npoints = 2*nx+1
    xl3 = [x0-nx, x0, x0+nx]
    nx = fix( abs(xc_center-xc_edge) )
    xc3 = [xc_center-nx, xc_center, xc_center+nx]

    xls = indgen(npoints, start=xl3[0], /float)
    ;; shrink the interval in continuum grid
    ;; to match 
    ;; - the given range
    ;; - npoint in line grid
    xcs = indgen(npoints, start=xc3[0], /float, increment=float(nx)/float(nxl))

    return, npoints
END

;------------------------------------------------------------------------
;; GIVEN THE CALCULATED GRID
;; INTERPOLATE IQUV PROFILE
;; s4, (nx,ny,4,1), 4 : I,Q,U,V, 1 : nseq
;; spil, (nx,npoints,4,1), 4 : I,Q,U,V, 1 : nseq
;; spic, (nx,npoints,4,1), 4 : I,Q,U,V, 1 : nseq
FUNCTION INTERP_PROFILE, s4, xls, xcs, spil=spil, spic=spic

    ss = size(s4)
    if  ss[0] ne 4 then throw_error, "ndim of s4 !=4"
    if  ss[3] ne 4 then throw_error, "size of the 3rd dimension of s4 !=4"
    if  ss[4] ne 1 then throw_error, "size of the 4th dimension of s4 !=1"
    nx = ss[1]
    ny = ss[2]
    xdata = indgen(ny)

    ;; interpolate line
    npoints = n_elements(xls)
    spil = fltarr(nx,npoints,4,1)
    for i=0,3 do begin
    for j=0,nx-1 do begin
        ydata = reform(s4[j,*,i,0])
        spil[j,*,i,0] = interpolate(ydata,xls,/cubic)
    endfor
    endfor
    ;; interpolate continuum
    npoints = n_elements(xcs)
    spic = fltarr(nx,npoints,4,1)
    for i=0,3 do begin
    for j=0,nx-1 do begin
        ydata = reform(s4[j,*,i,0])
        spic[j,*,i,0] = interpolate(ydata,xcs) ;; bilinear
    endfor
    endfor

    return, 1b

END

;------------------------------------------------------------------------
;; spil, (nx,npoints,4,nseq), 4 : I,Q,U,V
;; arr, (nx,npoints,3,2,nseq), 3 : Q,U,V, 2 : line,continuum
FUNCTION MAKE_HALF_ARRAY, spil
    
    ss = size(spil)
    if ss[0] ne 4 then throw_error, "ndim of spil !=4"
    npoints = ss[2]
    npoints_half = (npoints-1) / 2
    
    arr = fltarr(ss[1],npoints_half,3,2,ss[4])

    return, arr
END
;------------------------------------------------------------------------
;; error array given spil
FUNCTION MAKE_ERROR_ARRAY, spil, err
    
    arr = MAKE_HALF_ARRAY(spil)
    arr[*,*,*,*,*] = 1.0 * err

    return, arr
END

;------------------------------------------------------------------------
;; half arr, (nx,npoints,3,2,nseq), 3 : Q,U,V, 2 : line,continuum
;; weight array given spil
FUNCTION MAKE_WEIGHT_ARRAY, spil, wtQ=wtQ, wtU=wtU, wtV=wtV, wtL=wtL, wtC=wtC
    if not keyword_set(wtQ) then wtQ = 1.0
    if not keyword_set(wtU) then wtU = 1.0
    if not keyword_set(wtV) then wtV = 1.0
    if not keyword_set(wtL) then wtL = 1.0
    if not keyword_set(wtC) then wtC = 1.0

    arr = MAKE_HALF_ARRAY(spil)
    
    arr[*,*,*,*,*] = 1.0
    arr[*,*,0,*,*] *= wtQ
    arr[*,*,1,*,*] *= wtU
    arr[*,*,2,*,*] *= wtV
    arr[*,*,*,0,*] *= wtL
    arr[*,*,*,1,*] *= wtC

    return, arr
END
;------------------------------------------------------------------------
;; result/target array given spil and spic
;; half arr, (nx,npoints,3,2,nseq), 3 : Q,U,V, 2 : line,continuum
;; spil, (nx,npoints,4,nseq), 4 : I,Q,U,V
FUNCTION MAKE_RESULT_ARRAY, spil, spic, target=target, zero_netV = zero_netV, flat_cont=flat_cont

    arr = MAKE_HALF_ARRAY(spil)
    ;; diff between left and right of 
    ;; a (anti-)symmetry profile should be zero
    if keyword_set(target) then return, arr

    ss = size(arr) & nph = ss[2] & nx = ss[1] & nseq = ss[5]
    ;; Line : Q, U, symmetry
    for i=0,1 do begin
        arr[*,*,i,0,*] = spil[*,0:nph-1,i+1,*] + reverse(spil[*,nph+1:2*nph,i+1,*],2)
    endfor
    ;; Line : V, anti-symmetry
    arr[*,*,2,0,*] = spil[*,0:nph-1,3,*] + reverse(spil[*,nph+1:2*nph,3,*],2)
    if not keyword_set(zero_netV) then arr[*,*,2,0,*] -= spil[*,nph,3,*]

    ;; Continuum : zero
    for i=0,2 do begin
        arr[*,*,i,1,*] = 0.5*(spic[*,0:nph-1,i+1,*] - spic[*,nph+1:2*nph,i+1,*])
        if keyword_set(flat_cont) then arr[*,*,i,1,*] -= reform(mean(arr[*,*,i,1,*],dimension=2),nx,1,1,1,nseq)
    endfor

    return, arr
END


;------------------------------------------------------------------------
;; parinfo struct for mpfit
; create parinfo for a single parameter in mpfit
FUNCTION INIT_PARINFO 
    parinfo = {parinfo, value:0.0, fixed:0b, limited:[1b,1b], limits:[0.0,1.0], parname:'sc'}
    return, parinfo
END

; create parinfo for 5 parameters in mpfit
FUNCTION MAKE_PARINFO, xn, tn, xc, tc, sc, th_vs, conf_symfit
    npar = 6
    parinfo = [] & for i=0,npar-1 do parinfo = [parinfo,init_parinfo()]
    ;; xn
    i=0
    parinfo[i].value = xn
    parinfo[i].parname = 'xn'
    parinfo[i].limits = [-0.1,+0.1]
    ;; tn
    i=1
    parinfo[i].value = tn
    parinfo[i].parname = 'tn'
    parinfo[i].limits = [-90.0*!dtor,+90.0*!dtor]
    ;; xc
    i=2
    parinfo[i].value = xc
    parinfo[i].parname = 'xc'
    parinfo[i].limits = [-0.1,+0.1]
    ;; tc
    i=3
    parinfo[i].value = tc
    parinfo[i].parname = 'tc'
    parinfo[i].limits = [-90.0*!dtor,+90.0*!dtor]
    ;; sc
    i=4
    parinfo[i].value = sc
    parinfo[i].parname = 'sc'
    parinfo[i].limits = [-0.3,+0.3]
    parinfo[i].fixed = conf_symfit.fix_sc
    ;; th_vs
    i=5
    parinfo[i].value = th_vs
    parinfo[i].parname = 'th_vs'
    parinfo[i].limits = [-45.0,+45.0]
    parinfo[i].fixed = conf_symfit.fix_th_vs
    return,parinfo
END

;------------------------------------------------------------------------
;; spils, (nx,npoints,4,nseq), 4 : I,Q,U,V
;; dst3c, (nseq), array of struct
FUNCTION MODEL_PARDST, cx, par
    common symfit, spils, spics, dst3c

    xn=par[0] & tn=par[1] & xc=par[2] & tc=par[3] & sc=par[4] & th_vs=par[5]

    ss = size(spils) & nx = ss[1] & np=ss[2] & nst=ss[3] & ns=ss[4]
    nph = (np-1) / 2
    spmls = fltarr(nx,np,4,ns)
    spmcs = fltarr(nx,np,4,ns)
    if ns eq 1 then begin
        spmls = reform(spmls,nx,np,4,ns,/overwrite)
        spmcs = reform(spmcs,nx,np,4,ns,/overwrite)
    endif

    for i=0,ns-1 do begin
        dst = dst3c[i]
        mm = UPDATE_MMDST(dst, xn, tn, xc, tc, sc, th_vs=th_vs)
        spmls[*,*,*,i] = UPDATE_S3(reform(spils[*,*,*,i],nx,np,nst), mm)
        spmcs[*,*,*,i] = UPDATE_S3(reform(spics[*,*,*,i],nx,np,nst), mm)
    endfor
    res = MAKE_RESULT_ARRAY(spmls, spmcs, /zero_netV)


    print, "[SymFit] mean of Absolute error : ", mean(res)
    return, res
END
;------------------------------------------------------------------------
;; s4, (nx, ny, 4, nseq)
;; dst3, (3, 11, nseq)
FUNCTION MPFIT_PARDST, conf_symfit, s4, dst3, parinit, parinfo, err, weight, niter=niter, quiet=quiet
    common symfit, spils, spics, dst3c
    dst3c = dst3
    ss = size(s4)
    if ss[0] ne 4 then begin
    if ss[0] eq 3 then s4 = reform(s4,ss[1],ss[2],ss[3],1,/overwrite) else throw_error, "ndim of s4 == ",ss[0]
    endif
    ss = size(s4)
    ns = ss[4] & nx = ss[1] & ny = ss[2] & nst = ss[3]
    x0 = nx/2
    ;ret = CALCULATE_FIT_RANGE(460, 445, 120, 110, reform(s4[0,0,*,0]),xls=xls, xcs=xcs)
    ret = CALCULATE_FIT_RANGE(conf_symfit.iedgeL, conf_symfit.icentL, conf_symfit.iedgeC, conf_symfit.icentC, reform(s4[0,*,0,0]),xls=xls, xcs=xcs)
    ;ret = CALCULATE_FIT_RANGE(460, 445, 277, 267, reform(s4[0,0,*,0]),xls=xls, xcs=xcs)
    np = n_elements(xls)
    ;nph = (np-1)/2
    spils = fltarr(nx,np,4,ns)
    spics = fltarr(nx,np,4,ns)
    if ns eq 1 then begin
        spils = reform(spils,nx,np,4,ns,/overwrite)
        spics = reform(spics,nx,np,4,ns,/overwrite)
    endif
    for i=0,ns-1 do begin
        ret = INTERP_PROFILE(reform(s4[*,*,*,i],nx,ny,nst,1), xls, xcs, spil=spil, spic=spic)
        spils[*,*,*,i] = spil[*,*,*,*]
        spics[*,*,*,i] = spic[*,*,*,*]
    endfor
    
    ;vthres = 0.5
    ;nthres = 5
    ;mask  = fltarr(ns,nx)
    ;;for i=0,ns-1 do begin
    ;;endfor

    weights = MAKE_WEIGHT_ARRAY(spils,wtQ=weight.wtQ,wtU=weight.wtU,wtV=weight.wtV,wtL=weight.wtL,wtC=weight.wtC)
    
    if not keyword_set(niter) then niter = 20
    if not keyword_set(quiet) then quiet=1b

    errs =  MAKE_ERROR_ARRAY(spils, err)
    targets = MAKE_RESULT_ARRAY(spils, spics, /target)
    ;;stop
    ret = mpfitfun('MODEL_PARDST',0,targets,errs,parinit,weight=weights,errmsg=errmsg, $
				quiet=quiet,maxiter=niter,niter=k,yfit=sfit,bestnorm=eval1,parinfo=parinfo)


    return,ret

END

;------------------------------------------------------------------------
;; xpos = 118
;; sc = 0.0
;; xn = +0.00300
;; tn = -0.06286
;; xc = +0.02500
;; tc = +0.12566
;;[do not use, order of dimemsion not corrected]
FUNCTION TEST_MPFIT_PARDST, s_origin, dst, conf_symfit

    ss = size(s_origin)
    s4 = reform(s_origin, 1, ss[1], ss[2], ss[3])
    s4 = s4[*,80:150,*,*]

    ss = size(dst)
    dst3 = reform(dst, 1, ss[1], ss[2])

    telpos1 = ['WEST']

    parinit = [0.00300, -0.06286, 0.02500, 0.12566, 0.0]
    parinfo = MAKE_PARINFO(parinit[0], parinit[1], parinit[2], parinit[3], parinit[4])
    weight = {weight_struct, wtQ:2.0, wtU:2.0, wtV:1.0, wtL:1.0, wtC:2.0}
    p = MPFIT_PARDST(conf_symfit, s4, dst3, telpos1, parinit, parinfo, 2E-4, weight, quiet=0b, niter=100)
    return, p
END
