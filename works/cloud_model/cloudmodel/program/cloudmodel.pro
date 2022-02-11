function cloudmodel,pin,wl,bg,full=full

;+
;Name:
;	CLOUDMODEL
;
;Purpose:
;	Calculate contrasts of given parameters for any
;	wavelength range using Beckers' cloud model. 
;
;Calling Sequence:
;	Contrasts=cloudmodel(Pin,[wl,[bg]],[/full1],[/full2])
;
;Input Parameters:
;	Pin - A vector whose elements are as follows.
;	      Pin(0) : Source Function
;	      Pin(1) : Optical Depth
;	      Pin(2) : Doppler Width
;	      Pin(3) : Doppler Shift
;	wl  - Wavelengths expressed in difference
;	      from H alpha center (6562.808 Angstrom).
;	bg  - Background intensities corresponding to wl.
;	      Normalized by continuum. (can be omitted.)
;
;Keywords:
;	full - 2 : If set returns contrasts continuously for the
;		   range of H alpha +/- 2 Angstrom.
;	       4 : If set returns contrasts continuously for the
;		   range of H alpha +/- 4 Angstrom.
;
;History:
;       Written 24 Jan 2007, K. Otsuji
;-

if not isvalid(full) then full=0

case full of
    2:begin
        aaa=rd_tfile('~/idl_data/ref_Ha_filter_100.dat',2)
        wl=aaa(0,*)-6562.808d
        bg=aaa(1,*)
    end
    4:begin
        aaa=rd_tfile('~/idl_data/ref_Ha_filter_200.dat',2)
        wl=aaa(0,*)-6562.808d
        bg=aaa(1,*)
    end
    else:
endcase

if not isvalid(wl) then begin
    common share1,wl1,bg1
    wl=wl1
    bg=bg1
endif

if not isvalid(bg) then begin
    case 1 of
        max(abs(wl)) le 2.:aaa=double(rd_tfile('~/idl_data/ref_Ha_filter_100.dat',2))
        max(abs(wl)) le 4.:aaa=double(rd_tfile('~/idl_data/ref_Ha_filter_200.dat',2))
    endcase
    aaa(0,*)=aaa(0,*)-6562.808
    bg=interpol(aaa(1,*),aaa(0,*),wl)
endif

sf=pin(0)
t0=pin(1)
dw=pin(2)
ds=pin(3)

tau=t0*exp(-((wl-ds)/dw)^2)
C=[sf/bg-1]*(1-exp(-tau))

return,C

end
