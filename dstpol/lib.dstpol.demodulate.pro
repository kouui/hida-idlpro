
;; demodulate 2D spectra sequence

;********************************************************************
FUNCTION sp_demodulate,sps,nrot,th_offset,roll_shutter=roll_shutter
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
for i=0,nn-1 do begin
	s[*,*,0] = s[*,*,0] + sps[*,*,i]			; I
	s[*,*,1] = s[*,*,1] + sps[*,*,i] *cos(4*(th[i]+dth))	; Q
	s[*,*,2] = s[*,*,2] - sps[*,*,i] *sin(4*(th[i]+dth))	; U
	s[*,*,3] = s[*,*,3] - sps[*,*,i] *sin(2*(th[i]+dth))	; V
	s[*,*,4] = s[*,*,4] + sps[*,*,i] *cos(2*(th[i]+dth))	; R
endfor
s = s/nn

return,s

END

;********************************************************************
FUNCTION img3d_demodulate, img3d, expo, rotp, camera, del=del, $
            bin=bin, orca_if=orca_if, wl_order=wl_order

if not keyword_set(del) then del=127. ;;[deg]
if not keyword_set(wl_order) then wl_order=1

th_offset = get_th_offset(camera,rotp,expo,bin,cam_IF=orca_if)

print, '[qlpol] thoffset= ', string(th_offset, form='(f6.2)'),  '[deg]'

ss = size(img3d)
if ss[0] ne 3 then throw_error, "Need 3 dimensional array to perform demodulation"
nxp = ss[1] & ny = ss[2] & nn = ss[3]

nrot = expo * nn / float(rotp)

s = sp_demodulate(img3d,nrot,th_offset)

mal = 1-cos(del/180*!pi)
mac = sin(del/180*!pi)
;;s[*,*,0] = (sl[*,*,0]+sr[*,*,0])/2
s[*,*,1:2] /= 0.5*mal 	; divide mod. ampl. to get Q,U
s[*,*,3:4] /= 0.5*mac 	; divide mod. ampl. to get V

if wl_order eq 1 then begin	; swap l and r
	;;print,'reverse left and right, change sign of Q'
	s[*,*,1] = -s[*,*,1]
	s[*,*,2] = -s[*,*,2]
endif

case wl_order of
   0: rotang = +45. 
   1: rotang = -45. 
endcase

;-- rotate to align -Q in slit direction
phi2 = 2.*rotang/180*!pi
c2 = cos(phi2) &	s2 = sin(phi2)
q = s[*,*,1] &		u = s[*,*,2]
s[*,*,1] =  c2*q + s2*u
s[*,*,2] = -s2*q + c2*u

return, s
END
