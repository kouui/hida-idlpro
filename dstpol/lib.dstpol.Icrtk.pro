

;; correct I crosstalk for QUV

;********************************************************************
function correct_Icrosstk,s,get_coeffs=get_coeffs,coeffs=coeffs,yrange=yr,correct=correct
;-- I -> Q,U,V & bias correction, if needed, do after DST Mueller matrix is applied --
;  s[*,*,5]
;  coeffs[2,4]    P' = P -c0*I - c1
imgsize,s,nx,ny,nn
if not keyword_set(yr) then yr=[0,ny-1]

if keyword_set(get_coeffs) or not keyword_set(coeffs) then begin
	coeffs = fltarr(2,nn-1)
	for i=0,nn-2 do coeffs[*, i]= poly_fit(s[*,yr[0]:yr[1],0],s[*,yr[0]:yr[1],i+1],1)
endif

s2 = s
dic = ['','Q','U','V']
if keyword_set(correct) then begin
	for i=1,3 do begin
		pos = where(dic[i] eq correct, count)
		if count then begin
			coe = coeffs[*,i-1]
			s2[*,*,i] = s[*,*,i] - coe[1]*s[*,*,0] - coe[0]
			print,'I -> ',dic[i],'  ',coe
		endif	
	endfor
endif else begin
	for i=1,nn-1 do begin
		coe = coeffs[*,i-1]
		s2[*,*,i] = s[*,*,i] - coe[1]*s[*,*,0] - coe[0]
		print,'I -> ',dic[i],'  ',coe
	endfor
endelse

return,s2

end
