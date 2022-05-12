;+
; NAME       : tvscls.pro (procedure)
; PURPOSE :
;    tvscl with limit of n-sigma, or min-max
; CATEGORY :
;	idlpro/util
; CALLING SEQUENCE :
;	tvscls,img,x,y,sgm=s,vmin=vmin,vmax=vmax
; INPUTS :
;      	img[*,*]
;	x,y     -  position
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
;	sgm	-- ex, 3, [-3,4]
; MODIFICATION HISTORY :
;	K.I. 2012/05/29
;	K.I. 2019/05/19
;	K.I. 2021/08/15
;	K.I. 2021/09/10		bug fix.
;-
pro tvscls,img,x,y,sgm=s,vmin=vmin,vmax=vmax

if n_elements(x) eq 0 then x=0
if n_elements(y) eq 0 then y=0
if n_elements(s) eq 1 then s=s*[-1,1]

mi=min(img)
mx=max(img)
if n_elements(vmin) ne 0 then mi=vmin
if n_elements(vmax) ne 0 then mx=vmax
if keyword_set(s) then begin
	av=mean(img,/nan)
	dv=sqrt(total((img-av)^2,/nan)/n_elements(img))
	mi=av+s[0]*dv
	mx=av+s[1]*dv
endif

tv,(float((img-mi)/(mx-mi))*255)>0<255,x,y

end
