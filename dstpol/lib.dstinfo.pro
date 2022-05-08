; calc_dstpos
;    date_obs  ->  dst[*,11]
;  return dst[3,11]

;   2022.01.14	k.i., u.k.
;   2022.04.04	k.i., u.k.	incli of mandatory

;-----------------------------------------------------------------
function val2ang,val

deg = fix(val)
sec = fix((val-deg)*3600)
mm  = sec/60
sec = sec mod 60
return,[deg,mm,sec]

end

;-----------------------------------------------------------------
function calc_dstinfo,DATE_OBS,incli,telpos=telpos

; DATE_OBS= '2021-10-10T09:41:57.911' 
;  incli: angle between slit and sky north [deg.]

if not keyword_set(telpos) then telpos='WEST'

;  calc. solar height & azimuth 
;  ref. http://k-ichikawa.blog.enjoy.jp/etc/HP/js/sunShineAngle/ssa.html

lat = 36.25	; Hida
lon = 137.30
omg = 2*!pi/365.25
rad = 1./180.*!pi

yr = strmid(DATE_OBS,0,4)
mon = strmid(DATE_OBS,5,2)
day = strmid(DATE_OBS,8,2)
hh = strmid(DATE_OBS,11,2)
mm = strmid(DATE_OBS,14,2)
ss = strmid(DATE_OBS,17,2)
J = julday(mon,day,yr)-julday(1,1,yr)	; day of year

; ÔŒo [deg.]
del = 0.33281d - 22.984d * cos(omg*J) - 0.34990d * cos(2*omg*J) - 0.13980d *cos(3*omg*J) $
      + 3.7872d *sin(omg*J) + 0.03250d *sin(2*omg*J) + 0.07187d *sin(3*omg*J)
; ‹ÏŽž·
e = 0.0072d *cos(omg*J ) - 0.0528d *cos(2*omg*J ) - 0.0012d *cos(3*omg*J ) $
      - 0.1229d *sin(omg*J ) - 0.1565d *sin(2*omg*J ) - 0.0041d *sin(3*omg*J )

Ts = hh+mm/60.+ss/3600.
T = Ts + (lon - 135)/15 + e
th = 15.*T - 180.	; time [deg.]

h = asin(sin(lat*rad)*sin(del*rad) + cos(lat*rad)*cos(del*rad)*cos(th*rad))  ; ‚“x [rad]

sA = cos(del*rad)*sin(th*rad)/cos(h)
cA = (sin(h)*sin(lat*rad) - sin(del*rad))/cos(h)/cos(lat*rad)
A = atan(sA, cA)/rad 	; azimuth [deg.]
z = (!pi/2 - h)/rad	; zenith dist. [deg.]

ha = th/360.*24	; hr
if ha lt 0 then ha=ha+24.
if A lt 0 then A=A+360.
dst = intarr(3,11)
dst[*,0] = val2ang(ha)
dst[*,1] = val2ang(z)
dst[*,9] = val2ang(A)	
if incli lt 0 then incli=incli+360
dst[*,4] = val2ang(incli)
if telpos eq 'WEST' then dst[*,10]=[0,0,1] else dst[*,10]=[0,0,0]

return,dst

end

;********************************************************************
function arr2deg,dd
	rad = (dd[0]+dd[1]/60.+dd[2]/3600.)
	return,rad
end

;********************************************************************
function dst_st,dstarr
;; date_obs = fits_keyval(h,'DATE_OB2')
;; dst = dst_st(calc_dstinfo(date_obs,incli))

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
