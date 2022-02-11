pro mk_ref_ha,wv=wv,fw=fw,cen=cen,wra=wra,dw=dw

if not file_test('iss_current_a6563_hr.fits') then spawn, $
  'wget ftp://solis.nso.edu/synoptic/level2/iss/iss_current_a6563_hr.fits'

mreadfits,'iss_current_a6563_hr.fits',index,data,/quiet

if not isvalid(wv) then wv=[+0.0d] ;[A]
nf=n_elements(wv)
if not isvalid(fw) then fw=replicate(0.25d,nf) ;[A]
if not isvalid(cen) then cen=6562.8080d
if not isvalid(wra) then wra=[-4d,+4d] ;[A]
if not isvalid(dw) then dw=0.001d

ww=index.CDELT1*(dindgen(max(abs(wra))*2/index.CDELT1)- $
                 max(abs(wra))/index.CDELT1)
nw0=n_elements(ww)

gg=exp(-(rebin(ww,nw0,nf)/ $
         (rebin(transpose(fw),nw0,nf)/2/alog(2)))^2)

wave0=dindgen((wra[1]-wra[0])/dw)*dw+wra[0]+cen
nw1=n_elements(wave0)

int0=data[*,1]
int1=dblarr(n_elements(wave0),nf)
for i=0,nf-1 do int1[0,i]=interpol(convol(int0,gg[*,i],/norm,/edge_t), $
                                   data[*,0],wave0/10)

wave1=transpose(wave0)-cen
wave2=[wra,wv]
wave2=wave2(uniq(wave2,sort(wave2)))

int2=dblarr(n_elements(wave0))
wi=intarr(n_elements(wave2))
for i=0,n_elements(wave2)-1 do $
  wi[i]=where(abs(wave1-wave2[i]) eq min(abs(wave1-wave2[i])))

int2[wi[0]]=int1[wi[0]:wi[1],0]
for i=1,nf-1 do int2[wi[i]]= $
  int1[wi[i]:wi[i+1],i-1]*(1-findgen(wi[i+1]-wi[i])/(wi[i+1]-wi[i]))+ $
  int1[wi[i]:wi[i+1],i]*findgen(wi[i+1]-wi[i])/(wi[i+1]-wi[i])
int2[wi[i]]=int1[wi[i]:*,nf-1]
  
openw,1,'ref_ha.dat'
printf,1,[wave1,transpose(int2),transpose(int1)]
close,1

end


