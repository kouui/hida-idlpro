;  mmao.pro
;	return Mueler matrix of DST-AO
;   2022.05.05	k.i.
function mmao,wlt

;wlt = [1083.]

;mmspdat = 'D:\Projects\data\DSTpol\DST-AO_m20210819_134318_MM.sav'
mmspdat = '/share/Projects/data/DSTpol/DST-AO_m20210819_134318_MM.sav'

restore,mmspdat	; -> MM[4, 4, 2068], wl[2068]  in nm
nw = n_elements(wl)
nwt = n_elements(wlt)
mat = dblarr(4,4,nwt)
for i=0,3 do begin
for j=0,3 do begin
	mm1 = reform(mm[i,j,*]/mm[0,0,*],nw)
	mat[i,j,*] = interpol(mm1,wl,wlt)
endfor
endfor

return,mat

end
