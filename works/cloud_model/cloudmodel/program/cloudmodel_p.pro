function cloudmodel_p,pin,wl

;+
;Name:
;	CLOUDMODEL_P
;
;Purpose:
;	Calculate prominence intensity (normalized by continuum at
;	disk center) of given parameters for any
;	wavelength range using Beckers' cloud model. 
;
;Calling Sequence:
;	Contrasts=cloudmodel(Pin,wl)
;
;Input Parameters:
;	Pin - A vector whose elements are as follows.
;	      Pin(0) : Source Function
;	      Pin(1) : Optical Depth
;	      Pin(2) : Doppler Width
;	      Pin(3) : Doppler Shift
;	wl  - Wavelengths expressed in difference
;	      from H alpha center (6562.808 Angstrom).
;
;History:
;       Written 13 Jul 2011, K. Otsuji
;-

if not isvalid(wl) then begin
    common share1,wl1
    wl=wl1
endif

sf=pin(0)
t0=pin(1)
dw=pin(2)
ds=pin(3)

tau=t0*exp(-((wl-ds)/dw)^2)
C=sf*(1-exp(-tau))

return,C

end
