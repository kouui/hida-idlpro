;  library to calculate Mueller Matrix of DST
;
; History
; 2021.09.20  k.u.  initialized
; 2021.12.10  t.a.  added a keyword qin

;*************************************************************************
;throw_error (procedure)
;par_dst (function)
;muellermatrix_mirr (function)
;muellermatrix_wp (function)
;muellermatrix_rot (finction)
;mm_dst (function)
;*************************************************************************

;*************************************************************************
;+
; NAME       : throw_error (procedure)
; PURPOSE    :
;   throw error with an text discription
; CALLING SEQUENCE :
;        throw_error, text
; INPUTS :
;        text    - text discription, string
; MODIFICATION HISTORY :
;        k.u. '2021/09/20'    
;*************************************************************************

pro THROW_ERROR, text
   on_error, 2   ; Stop in caller
   MESSAGE, LEVEL=-1, text
end

;*************************************************************************
;+
; NAME       : par_dst (function)
; PURPOSE :
; 	parameters of DST instrumental polarization
; CALLING SEQUENCE :
;        res=par_dst(wave,telpos)
; INPUTS :
;        wave    - wavelength, float, [A]
;        telpos  - telescope position, string, 'WEST' or 'EAST'
; OUTPUT :
;        res -- structure of result
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/11/23'		
;        T.A. '2016/11/03'    parameters for HSP	
;        T.A. '2017/01/16'    keyword th_mmsp2_hsp	
;        k.u. '2021/09/20'    polarimeterlib_v2.pro -> mmdst.pro
;*************************************************************************
function par_dst, wave, telpos


; 0 for WEST position
; 1 for EAST position
ref=[$
      ;[0,10830.,-0.0236,  9.544*!dtor, 0.0219, 5.566*!dtor,0.0000],$ ;20120429
      [0,10830.,-0.0236, -9.547*!dtor, 0.0219,-5.561*!dtor,0.0000],$ ;20120429
      [0,10050.,-0.0275, -9.713*!dtor, 0.0270, 2.268*!dtor,0.0045],$ ;20120614
      [0, 9016.,-0.0417, -8.646*!dtor, 0.0246,16.335*!dtor,0.0499],$ ;20120614
      [0, 8662.,-0.0477, -9.878*!dtor, 0.0153,21.319*!dtor,0.0035],$ ;20120614
      [0, 8542.,-0.0512,-11.038*!dtor, 0.0113,23.180*!dtor,0.0200],$ ;20120614
      [0, 8498.,-0.0505,-10.610*!dtor, 0.0097,23.454*!dtor,0.0078],$ ;20120614
      [0, 8392.,-0.0505,-10.262*!dtor, 0.0062,24.779*!dtor,0.0005],$ ;20120614
      [0, 6563.,-0.0419,-17.057*!dtor,-0.0399,29.294*!dtor,0.0096],$ ;20120625
      [1, 6563.,-0.0417,-15.707*!dtor,-0.0392,30.443*!dtor,0.0084],$ ;20120625
      [0, 6303.,-0.0406,-17.693*!dtor,-0.0385,26.178*!dtor,0.0146],$ ;20120625
      [1, 6303.,-0.0407,-16.301*!dtor,-0.0378,27.453*!dtor,0.0041],$ ;20120625
      [0, 5890.,-0.0371,-19.969*!dtor,-0.0368,20.876*!dtor,0.0227],$ ;20120625
      [1, 5890.,-0.0390,-17.711*!dtor,-0.0340,21.373*!dtor,0.0000],$ ;20120625
      [0, 5100.,-0.0369,-21.639*!dtor,-0.0271, 7.647*!dtor,0.0177],$ ;20120625
      [1, 5100.,-0.0374,-20.579*!dtor,-0.0276, 9.103*!dtor,0.0000],$ ;20120625
      [0, 4861.,-0.0372,-22.886*!dtor,-0.0252, 1.075*!dtor,0.0405],$ ;20120625
      [1, 4861.,-0.0365,-21.858*!dtor,-0.0251, 2.737*!dtor,0.0000],$ ;20120625
      [0, 4340.,-0.0394,-24.283*!dtor,-0.0212,-9.910*!dtor,0.0548],$ ;20120625
      [1, 4340.,-0.0400,-22.434*!dtor,-0.0265,-7.655*!dtor,0.0000],$ ;20120625
      [0, 4101.,-0.0440,-25.292*!dtor,-0.0211,-14.340*!dtor,0.1317]$ ;20120625
      ;[0,10050.,-0.0274,  9.713*!dtor, 0.0270,-2.268*!dtor,0.0045],$ ;20120614&correct V
      ;[0, 9016.,-0.0417,  8.646*!dtor, 0.0246,-16.335*!dtor,0.0499],$ ;20120614
      ;[0, 8662.,-0.0477,  9.878*!dtor, 0.0153,-21.319*!dtor,0.0199],$ ;20120614
      ;[0, 8542.,-0.0512, 11.038*!dtor, 0.0113,-23.180*!dtor,0.0200],$ ;20120614
      ;[0, 8498.,-0.0505, 10.610*!dtor, 0.0097,-23.454*!dtor,0.0229],$ ;20120614
      ;[0, 8392.,-0.0505, 10.262*!dtor, 0.0062,-24.779*!dtor,0.0213],$ ;20120614
      ;[0, 6563.,-0.0419, 17.057*!dtor,-0.0399,-29.294*!dtor,0.0096],$ ;20120625
      ;[1, 6563.,-0.0417, 15.707*!dtor,-0.0392,-30.443*!dtor,0.0084],$ ;20120625
      ;[0, 6303.,-0.0406, 17.693*!dtor,-0.0385,-26.178*!dtor,0.0146],$ ;20120625
      ;[1, 6303.,-0.0407, 16.301*!dtor,-0.0378,-27.453*!dtor,0.0041],$ ;20120625
      ;[0, 5890.,-0.0371, 19.969*!dtor,-0.0368,-20.876*!dtor,0.0227],$ ;20120625
      ;[1, 5890.,-0.0390, 17.711*!dtor,-0.0340,-21.373*!dtor,0.0000],$ ;20120625
      ;[0, 5100.,-0.0369, 21.639*!dtor,-0.0271, -7.647*!dtor,0.0177],$ ;20120625
      ;[1, 5100.,-0.0374, 20.579*!dtor,-0.0276, -9.103*!dtor,0.0000],$ ;20120625
      ;[0, 4861.,-0.0372, 22.886*!dtor,-0.0252, -1.075*!dtor,0.0405],$ ;20120625
      ;[1, 4861.,-0.0365, 21.858*!dtor,-0.0251, -2.737*!dtor,0.0000],$ ;20120625
      ;[0, 4340.,-0.0394, 24.283*!dtor,-0.0212,  9.910*!dtor,0.0548],$ ;20120625
      ;[1, 4340.,-0.0400, 22.434*!dtor,-0.0265,  7.655*!dtor,0.0000],$ ;20120625
      ;[0, 4101.,-0.0440, 25.292*!dtor,-0.0211, 14.340*!dtor,0.1317]$ ;20120625
    ]

if telpos eq 'WEST' then pos=where(ref[0,*] eq 0) else pos=where(ref[0,*] eq 1)
res={par_dst,$
     xn      : interpol(ref[2,pos],ref[1,pos],wave),$
     tn      : interpol(ref[3,pos],ref[1,pos],wave),$
     xc      : interpol(ref[4,pos],ref[1,pos],wave),$   
     tc      : interpol(ref[5,pos],ref[1,pos],wave),$
     sc      : interpol(ref[6,pos],ref[1,pos],wave),$ 
     t_en    : 0.    ,$
     dlen    : 0.    ,$
     t_ex    : 0.    ,$
     dlex    : 0.     $ 
     }

return,res
END

;*************************************************************************
;+
; NAME       : muellermatrix_mirr (function)
; PURPOSE :
; return normalized Mueller matrix for a mirror reflection
;positive Q-direction is in the plane of incidence
; CATEGORY :
;        idlpro/optic/ray/lib
; CALLING SEQUENCE :
;        mat = muellermatrix_mirr(tau,ro,/gen)
; INPUTS :
;       tau  --  retardation of the waveplate part of Mirror Model, float, (rad.)
; 	    ro   --  degree of polarization of the linear polarizer part of Mirror Model, float (-)
;       //delta,X are function of the incident angle,N=n+ik.
; OUTPUT :
; OPTIONAL INPUT PARAMETERS :
;       gen     -- general form 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '09/08/24      ; Stenflo "Solar Magnetic Field", p320.
;        T.A. '11/06/14      ; general form
;        T.A. '15/01/31      ; abs(ro)
;        K.U. '21/09/20'     ; polarimeterlib_v2.pro -> mmdst.pro
;*************************************************************************

function muellermatrix_mirr,tau,ro,gen=gen

tau	= float(tau)
ro	= float(ro)

if not keyword_set(gen) then begin
    mat	= 0.5*[ $
              [ro^2+1.,  ro^2-1.,               0.,              0.],     $
	  	      [ro^2-1.,  ro^2+1.,               0.,              0.],     $
              [0.,            0.,  -2.*ro*cos(tau), +2.*ro*sin(tau)],     $
  	  	      [0.,            0.,  -2.*ro*sin(tau), -2.*ro*cos(tau)]      $
    		  ]     
endif else begin
    ;mat = 1./(1.+ro)*[$
    mat = 1./(1.+abs(ro))*[$
                          [1.,ro,0.,0.],$
                          [ro,1.,0.,0.],$
                          [0.,0.,-sqrt(1.-ro^2)*cos(tau),-sqrt(1.-ro^2)*sin(tau)],$
                          [0.,0.,+sqrt(1.-ro^2)*sin(tau),-sqrt(1.-ro^2)*cos(tau)] $
                          ]
endelse


return,mat

end

;*************************************************************************
;+
; NAME       : muellermatrix_wp (function)
; PURPOSE :
; 	return Mueller matrix of linear retarder
; CATEGORY :
;        idlpro/optic/raylib
; CALLING SEQUENCE :
;        mat = muellermatrix_wp(del,phai,/jones,ref=ref,thick=thick,wv=wv)
; INPUTS :
; 	del  --  retardance, float, (rad.)
; 	phai --  angle of the axis, float, (rad., counter clockwise)
;	jones -  return Jones vector
;       ref  --  reflective index
;       thick -  thickness of wave plate (mm)
;       wv    -  wave length (nm)
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        k.i. '95/10/26     from lwp.pro
;        k.i. '96/02/17		jones keyword
;        T.A. '10/03/16		ref,thick,wv keyword
;        T.A. '10/04/07		modificate L61 and keywords(thck,wvl)
;        k.u. '21/09/20'    polarimeterlib_v2.pro -> mmdst.pro
;*************************************************************************
function muellermatrix_wp,del,phai,jones=jones,ref=ref,thick=thck,wv=wvl

if not keyword_set(jones) then begin	; Mueller matrix
    c2 = cos(2.*phai)                                                      
    s2 = sin(2.*phai)  
    cd = cos(del)
    sd = sin(del)

    if not keyword_set(ref) then begin	; no reflection
 	mat=[   [ 1.,	0.,		        0.,	            0.	    ],	$
      	    [ 0.,	c2^2+s2^2*cd,	s2*c2*(1.-cd),	-s2*sd	],	$
            [ 0.,	s2*c2*(1.-cd),	s2^2+c2^2*cd,	c2*sd   ],	$
            [ 0.,	s2*sd,		    -c2*sd,		    cd	    ] ]
    endif else begin	; reflection
    	if (not keyword_set(thck)) or (not keyword_set(wvl)) then begin
            print,'you must input two keywords "thick [mm]" and "wv [nm]"'
            mat = -1
        endif else begin
            ref=ref*1.
            thick=thck*10.^(-3)	;[m]		
            wv=wvl*10.^(-9)		;[m]
            rr  = 2.*((1.-ref)/(1.+ref))^2
            clm = cos(4.*!pi*thick*ref/wv)
            slm = sin(4.*!pi*thick*ref/wv)
            c2d = cos(2.*del)	
            s2d = sin(2.*del)	
            f11 = rr*clm*cd+1.
            f12 = -1.*rr*slm*sd
            f33 = rr*clm*c2d+cd
            f43 = -rr*clm*s2d-sd

            mat0=[ [	f11,	f12*c2,			f12*s2,			0.	],	$
                  [ f12*c2,	f11*c2^2+f33*s2^2,	s2*c2*(f11-f33),	f43*s2],	$
                  [ f12*s2,	s2*c2*(f11-f33),	f11*s2^2+f33*c2^2,	-f43*c2	],	$
                   [	0.,	-f43*s2,		f43*c2,	        	f33	] ]
            mat = (1.-(1.-ref)^2/(1.+ref)^2)^2 *mat0
        endelse
    endelse

endif else begin	; Jones matrix
    c1=cos(phai)
    s1=sin(phai)
    i=complex(0,1)

    if not keyword_set(ref) then begin	; no reflection
    	edel=exp(-i*del)	; sign ok?
    	m11=c1^2+edel*s1^2
    	m22=s1^2+edel*c1^2
    	m12=(1.-edel)*c1*s1
    	mat=[ [ 	m11,	m12 ],	$
		      [	    m12,	m22 ] ]
    ;mat=mat*conj(m11)/abs(m11)
    endif else begin	; reflection
    	if (not keyword_set(thck)) or (not keyword_set(wvl)) then begin
            print,'you must input two keywords "thick [mm]" and "wv [nm]"'
            mat = -1
        endif else begin
            ref  = ref*1.
            thick= thck*10.^(-3)	;[m]
            wv   = wvl*10.^(-9)		;[m]
            lx   = (4.*!pi*thick*ref/wv+del)*0.5    ; x is fast axis
            ly   = (4.*!pi*thick*ref/wv-del)*0.5
            rr   = (1.-ref)^2/(1.+ref)^2
            fx   = rr*exp(3.*i*lx)+exp(i*lx)
            fy   = rr*exp(3.*i*ly)+exp(i*ly)
            m11  = fx*c1^2+fy*s1^2
            m22  = fx*s1^2+fy*c1^2
            m12  = fx*s1*c1-fy*c1*s1
            
            mat0  = [ [ m11,	m12 ],	$
		     [	m12,	m22 ] ]
            mat = (1-(1.-ref)^2/(1.+ref)^2) *mat0
        endelse
    endelse
endelse

return,mat
end

;*************************************************************************
;+
; NAME       : muellermatrix_rot (finction)
; PURPOSE :
; 	return Mueller matrix for axis rotation
; CALLING SEQUENCE :
;        mat = muellermatrix_rot(phai)
; INPUTS :
; 	phai --  angle of axis rotation 
;		(rad., counterclockwise when we view towards the sun)
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;      T.A. '09/08/23
;      k.u. '21/09/20'    polarimeterlib_v2.pro -> mmdst.pro
;*******************************************************************
function muellermatrix_rot,phai

c2=cos(2.*phai)
s2=sin(2.*phai)
mat=[$
      [1.,	0.,	 0.,	0.],	$
      [0.,	+c2, +s2,	0.],	$
      [0.,	-s2, +c2,	0.],	$
      [0.,  0.,	 0.,	1.] ]

return,mat

end

;*******************************************************************
;+
; NAME       : mm_dst (function)
; PURPOSE :
; 	return Mueller matrix for DST model
;	S_out = mm_dst ## S_in
;	S_in +Q is in E-W direction of the plane of sky, S_out +Q is perpendicular to VS slit
; CALLING SEQUENCE :
;        res=mm_dst(zd, ha, az, incli, telpos, xn, tn, xc, tc, $
;                   sc=sc,delta_en=delta_en,t_en=t_en,delta_ex=delta_ex,t_ex=t_ex,/hsp)
; INPUTS :
;   zd     - zenith distance, float, (rad)
;   ha     - hour angle, float, (rad)
;   az     - azimuth, float, (rad)
;   incli  - inclination, float, (rad)
;   telpos - telescope position, string, 'EAST' or 'WEST'
;   xn     - degree of polarization of the linear polarizer part of Mirror Model of Newton mirror, float (-)
;   tn     - retardation of the waveplate part of Mirror Model of Newton mirror, float, (rad.)
;   xc     - degree of polarization of the linear polarizer part of Mirror Model of Cude mirror, float (-)
;   tc     - retardation of the waveplate part of Mirror Model of Cude mirror, float, (rad.)
; OUTPUT :
;   mat    - 4x4 matrix of the Meuller Matrix of DST
; OPTIONAL INPUT PARAMETERS : 
;   sc        - portion of stray light, float, (-)
;   delta_en  - retardation of entrance window as a waveplate, float, (rad)
;   t_en      - angle of axis of entrance window, float, (rad)
;   delta_ex  - retardation of exist window as a waveplate, float, (rad)
;   t_ex      - angle of axis of exist window, float, (rad)
;   hsp       - if `hsp` is set, then horizontal spectro-polarimeter, else, vertical spectro-polarimeter
;   qin       - if qin is 'slit', +Q on the sun is perpendicular to the slit
;             
; MODIFICATION HISTORY :
;      T.A. '11/06/14
;      T.A. '12/06/03  phi_N=za => -za
;      T.A. '12/06/16  sign
;      T.A. '13/08/04  keyword newton
;      T.A. '13/08/17  sign of zd
;      T.A. '16/09/09  horizontal spectropolarimeter, imgrot
;      T.A. '16/11/02  add keywords zd, ha, wave, and az
;      T.A. '16/11/03  version 2
;      T.A. '17/01/16  revive DST/VS (version 0)
;      T.A. '17/01/18  version 3, MMSP2(IR&MIRR) measured 2017.01.03 
;      k.u. '21/09/20' polarimeterlib_v2.pro -> mmdst.pro
;      T.A. '21/12/10  added a keyword 'qin'
function mm_dst, zd, ha, az, incli, telpos, $
                xn, tn, xc, tc,             $
                sc=sc,delta_en=delta_en,t_en=t_en,delta_ex=delta_ex,t_ex=t_ex, $
                hsp=hsp,qin=qin

lat=36.252/!radeg		;latitude of Hida observatory

; initilization of keyword argument
if not keyword_set(sc)       then sc = 0.
if not keyword_set(delta_en) then delta_en = 0.
if not keyword_set(t_en)     then t_en = 0.
if not keyword_set(delta_ex) then delta_ex = 0.
if not keyword_set(t_ex)     then t_ex = 0.
if not keyword_set(qin)      then qin = 'slit'

; definition of +Q on the sun
if qin eq 'slit' then begin
	; +Q on the sun is perpendicular to the slit
	R_sun2ew=muellermatrix_rot(-incli)
endif else begin
	; +Q on the sun is east-west on celestial sphere
	R_sun2ew=[$
	        [1.,0.,0.,0.],  $
        	[0.,1.,0.,0.],  $
        	[0.,0.,1.,0.], $
        	[0.,0.,0.,1.]  $
        	]
endelse


zd = abs(zd)
za = asin(cos(lat)*sin(ha)/sin(zd))
phi_N = za

if keyword_set(hsp) then begin
    
    if telpos eq 'WEST' then begin
        phi_C = -zd
        phi_v=+zd-za+incli
	endif else begin
	    phi_C=+zd
	    phi_v=-zd-za+incli
    endelse
    
endif else begin
    
    phi_v=(-az+!pi)
    phi_v=+zd-za+incli
    if telpos eq 'WEST' then phi_C = -zd else phi_C = +zd

endelse

M_S=[$
	[1.+sc ,0.,0.,0.],	$
	[0.    ,1.,0.,0.],	$
	[0.    ,0.,1.,0.],	$
	[0.    ,0.,0.,1.]	$
	]

M_p=[$
	[1.,0.,0.,0.],	$
	[0.,1.,0.,0.],	$
	[0.,0.,-1.,0.],	$
	[0.,0.,0.,-1.]	$
	]

M_G=M_p
M_N=muellermatrix_mirr(tn,xn,/gen)
M_C=muellermatrix_mirr(tc,xc,/gen)
D_en=Muellermatrix_WP(delta_en,t_en)
D_ex=Muellermatrix_WP(delta_ex,t_ex)
R_N=muellermatrix_rot(phi_N)
R_C=muellermatrix_rot(phi_C)
R_pl=muellermatrix_rot(phi_v)

mat = m_s ## R_pl ## D_ex ## M_C ## M_G ## R_C ## M_N ## M_P ## D_en## R_N ## R_sun2ew
;stop
if keyword_set(hsp) then begin
    THROW_ERROR, "Meuller Matrix for image rotator in hsp is not yet calibrated!!"
endif

return, mat

END
