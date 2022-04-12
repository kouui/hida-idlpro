

date = '20220316'
identifier = 'ircamera-experiment'
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'+date+'.'+identifier+'/'
charsize=1.5


;gotolabel = 'polarizer'
gotolabel = 'polarizer 0317'
;gotolabel = 'polarizer 0317 check frame'

case gotolabel of 
    'polarizer'    : goto, label_polarizer
    'polarizer 0317'    : goto, label_polarizer_0317
    'polarizer 0317 check frame'    : goto, label_polarizer_0317_check_frame
endcase
goto, label_the_end


label_polarizer : 


scale_fac = 2
camera = 'gold'

root = '/mnt/HDD3TBn51/DST/vs/'+date+'/'
folder = root + camera + '/'
print, 'processing folder : ', folder

fdatas = findfile(folder+'/exp3_*.fits')
fdarks = findfile(folder+'/dark_e0015*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'
nf =  n_elements(fdatas)

print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2] & nt=ss[3]
dark = reform(rebin(dark, nx, ny, 1))

__tmp = readfits(fdatas[0],hd)
ss = size(__tmp) & nx = ss[1] & ny = ss[2] & nt = ss[3]


print, "making modulation 2d map"
x1 = 100 & x2 = 150
if camera eq 'gold' then begin
    x1 = 125 & x2 = 175
endif
y1 = 256 & y2 = 256

curve = dblarr(nt,nf)
for i=0, nf-1 do begin
    data = float(readfits(fdatas[i],hd, /silent))
    for k=0, nt-1 do data[*,*,k] = data[*,*,k] - dark
    curve[*,i] = reform(rebin(data[x1:x2,y1:y2,*],1,1,nt))
endfor

wid = 1
window, wid, xs=nt * scale_fac, ys=nf * scale_fac
tvscl, rebin(curve,nt * scale_fac, nf * scale_fac)
fname = savedir + '/pol/' + camera + '.pol.0316.png'
WRITE_PNG, fname, TVRD(/TRUE)
print, 'saved as : ', fname

stop

label_polarizer_0317:

;scale_fac = 1
camera = 'gold'

root = '/mnt/HDD3TBn51/DST/vs/'+'20220317'+'/'
;version = '15ms'
version = '15ms1000'
;version = '15ms1000.v2'
;folder = root + camera + '.pol/' + '15ms/'
folder = root + camera + '.pol/' + version + '/'
print, 'processing folder : ', folder

case camera of 
    'flir' : fdatas = findfile(folder+'/flir*.fits')
    'gold' : fdatas = findfile(folder+'/vimba*.fits')
endcase
fdarks = findfile(folder+'/dark*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'
nf =  n_elements(fdatas)
case version of 
    '15ms' : nf = 100
    '15ms1000' : nf = 1000
    '15ms1000.v2' : nf = 1000
endcase

print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2] & nt=ss[3]
dark = reform(rebin(dark, nx, ny, 1))

__tmp = readfits(fdatas[0],hd)
ss = size(__tmp) & nx = ss[1] & ny = ss[2] & nt = ss[3]

print, "making modulation 2d map"
x1 = 100 & x2 = 150
if camera eq 'gold' then begin
    x1 = 125 & x2 = 175
endif
y1 = 256 & y2 = 256

;nf = 100
curve = dblarr(nt,nf)
for i=0, nf-1 do begin
    data = float(readfits(fdatas[i],hd, /silent))
    for k=0, nt-1 do data[*,*,k] = data[*,*,k] - dark
    curve[*,i] = reform(rebin(data[x1:x2,y1:y2,*],1,1,nt))
endfor

wid = 1
window, wid, xs=nt, ys=nf
tvscl, curve
fname = savedir + '/pol/' + camera + '.pol.0317.' + version + '.png'
WRITE_PNG, fname, TVRD(/TRUE)
print, 'saved as : ', fname

stop

label_polarizer_0317_check_frame:

camera = 'gold'

root = '/mnt/HDD3TBn51/DST/vs/'+'20220317'+'/'
;folder = root + camera + '.pol/' + '15ms/'
folder = root + camera + '.pol/' + '15ms1000/'
print, 'processing folder : ', folder

case camera of 
    'flir' : fdatas = findfile(folder+'/flir*.fits')
    'gold' : fdatas = findfile(folder+'/vimba*.fits')
endcase
fdarks = findfile(folder+'/dark*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
print, 'found ', n_elements(fdarks), ' dark files'
nf =  n_elements(fdatas)
nf = 200

print, 'making darks ...'
dark = float(readfits(fdarks[0],hd))
ss = size(dark) & nx=ss[1] & ny=ss[2] & nt=ss[3]
dark = reform(rebin(dark, nx, ny, 1))

__tmp = readfits(fdatas[0],hd)
ss = size(__tmp) & nx = ss[1] & ny = ss[2] & nt = ss[3]

print, "making frames ..."
x1 = 100 & x2 = 150
if camera eq 'gold' then begin
    x1 = 125 & x2 = 175
endif
y1 = 256 & y2 = 256

wid = 1
window, wid, xs=nx, ys=ny
for i=70, 90-1 do begin
    data = float(readfits(fdatas[i],hd, /silent))
    for k=0, nt-1 do data[*,*,k] = data[*,*,k] - dark
    data[0,0,0] = 0
    data[1,0,0] = 800
    tvscl, data[*,*,0] > 0 < 800
    fname = savedir + '/pol/0317.1000check/' + camera + strtrim(i,2) + '.png'
    WRITE_PNG, fname, TVRD(/TRUE)
    print, 'saved as : ', fname
endfor
stop

label_the_end : 
END