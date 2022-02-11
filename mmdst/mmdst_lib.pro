
; mmdst_lib
;  2022.02.06      u.k.    force qin='slit' in UPDATE_MMDST; added CORRECT_ICRTK
;  2022.02.11      u.k.    dst array -> dst struct
@mmdst

;------------------------------------------------------------------------
;; UPDATE mmdst
FUNCTION UPDATE_MMDST, dst, xn, tn, xc, tc, sc, th_vs=th_vs

;;	zd = (dst[0,1]+dst[1,1]/60.+dst[2,1]/3600.)/180.*!pi
;;	ha = (dst[0,0]+dst[1,0]/60.+dst[2,0]/3600.)/24.*2*!pi	; [hh,mm,ss] -> rad
;;	if ha gt !pi then ha = ha-2*!pi
;;	az = (dst[0,9]+dst[1,9]/60.+dst[2,9]/3600.)/180.*!pi
;;	incli = (dst[0,4]+dst[1,4]/60.+dst[2,4]/3600.)/180.*!pi

  ;;  mm = mm_dst(zd, ha, az, incli, telpos, xn, tn, xc, tc, sc=sc, qin='slit')
  	mm = mm_dst(dst.zd, dst.ha, dst.az, dst.incli, dst.pos, xn, tn, xc, tc, sc=sc, qin='slit')
    if keyword_set(th_vs) then mm = muellermatrix_rot(th_vs*!dtor) ## mm

    return, mm
END

;------------------------------------------------------------------------
;; UPDATE PROFILES
;; s3 (nx, ny, 4), 4 : I,Q,U,V
FUNCTION UPDATE_S3, s3, mm
   
   rmm = invert(mm)
   ss = size(s3) & nx= ss[1] & ny = ss[2]

   sm = fltarr(nx,ny,ss[3])
   for j=0,3 do begin
		sm[*,*,j] = rmm[0,j]*s3[*,*,0]
		for i=1,3 do sm[*,*,j] = sm[*,*,j] + rmm[i,j]*s3[*,*,i]
	endfor
   return, sm
END

;------------------------------------------------------------------------

FUNCTION CORRECT_ICRTK,s,get_coeffs=get_coeffs,coeffs=coeffs, yr=yr
;-- I -> Q,U,V & bias correction, if needed, do after DST Mueller matrix is applied --
;  s[*,*,4], (nx,ny,nst)
;  coeffs[2,4]    P' = P -c0*I - c1
imgsize,s,nx,ny,nn

if not keyword_set(yr) then yr=[0,nx-1]

if keyword_set(get_coeffs) or not keyword_set(coeffs) then begin
	coeffs = fltarr(2,nn-1)
	for i=0,nn-2 do coeffs[*, i]= poly_fit(s[*,yr[0]:yr[1],0],s[*,yr[0]:yr[1],i+1],1)
endif
print,"correct_Icrosstk coeffs : ", coeffs

s2 = s
for i=1,nn-1 do begin
	coe = coeffs[*,i-1]
	s2[*,*,i] = s[*,*,i] - coe[1]*s[*,*,0] - coe[0]
endfor

return,s2

END
