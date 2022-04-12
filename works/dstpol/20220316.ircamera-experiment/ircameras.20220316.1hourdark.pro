
;        ['white'  , 'green'  , 'red'    , 'yellow']
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x

date = '20220316'
identifier = 'ircamera-experiment'
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'+date+'.'+identifier+'/'
savedir += 'one-hour-darks/'
charsize=1.5

gotolabel = '1 hour darks'
gotolabel = '1 hour darks 0317'

case gotolabel of 
    '1 hour darks' : goto, label_one_hour_darks
    '1 hour darks 0317' : goto, label_one_hour_darks_0317
endcase
goto, label_the_end

label_one_hour_darks : 

camera = 'gold'

root = '/mnt/HDD3TBn51/DST/vs/'+date+'/'
folder = root + camera + '/'
print, 'processing folder : ', folder


fdatas = findfile(folder+'/exp2_*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
nf =  n_elements(fdatas)
__tmp = readfits(fdatas[0],hd)
ss = size(__tmp) & nx = ss[1] & ny = ss[2]

wid = 1
zero_arr = ulonarr(nf)
;window, wid, xs=nx, ys=ny
for i=0, nf-1 do begin
    img = readfits(fdatas[i],hd, /silent)
    pos = where(img gt 4*median(img))
    if total(pos) < 0 then continue
    print, "# ", i, " -> mean=", mean(img), " median=", median(img), " minmax=", minmax(img)
    ;tvscl, img
    ;fname = savedir  + camera + '/' + strtrim(i,2) + '1hour-darks.png'
    ;WRITE_PNG, fname, TVRD(/TRUE)
    
    ;; find zeros values
    pos = where(img eq 0, count)
    zero_arr[i] = count
endfor
;wdelete, wid
wid = 2
window, wid, xs=800, ys=600
plot, indgen(nf), zero_arr, charsize=charsize, xtitle="time [sec]", ytitle="# of zeros"

stop

label_one_hour_darks_0317:

version = '0317'
version = '0318'

camera = 'gold'

case version of 
    '0317' : begin
        root = '/mnt/HDD3TBn51/DST/vs/20220317/'
        folder = root + 'gold.dark1hour/'
    end
    '0318' : begin
        root = '/mnt/HDD3TBn51/DST/vs/20220318/'
        folder = root + 'gold.1hourdark/'
    end
endcase

print, 'processing folder : ', folder


fdatas = findfile(folder+'/*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
nf =  n_elements(fdatas)
__tmp = readfits(fdatas[0],hd)
ss = size(__tmp) & nx = ss[1] & ny = ss[2]
wid = 1
zero_arr = ulonarr(nf)
abnormals = ulonarr(nf)
mean_arr = fltarr(nf)
stdv_arr = fltarr(nf)
;window, wid, xs=nx, ys=ny
for i=0, nf-1 do begin
    img = readfits(fdatas[i],hd, /silent)
    mean_arr[i] = mean(img)
    stdv_arr[i] = stddev(img) 
    pos = where(img gt 4*median(img))
    if total(pos) < 0 then continue
    ;print, "# ", i, " -> mean=", mean(img), " median=", median(img), " minmax=", minmax(img)
    abnormals[i] = n_elements(pos)
    
    ;; find zeros values
    pos = where(img eq 0, count)
    zero_arr[i] = count

    ;; get mean
endfor
;wdelete, wid
wid = 2
window, wid, xs=800, ys=600
plot, indgen(nf), zero_arr, charsize=charsize, xtitle="time [sec]", ytitle="count", yr=[-1,1];, xr=[0,3600]
oplot, indgen(nf), abnormals, color=colors[1], line=2

fname = savedir  + camera + '.1hour-darks.zero.v'+version+'.png'
WRITE_PNG, fname, TVRD(/TRUE)

wid = 3
window, wid, xs=800, ys=600
plot, mean_arr, charsize=charsize, xtitle="time [sec]", ytitle="mean"

fname = savedir  + camera + '.1hour-darks.mean.v'+version+'.png'
WRITE_PNG, fname, TVRD(/TRUE)

wid = 4
window, wid, xs=800, ys=600
plot, stdv_arr, charsize=charsize, xtitle="time [sec]", ytitle="stddev"

fname = savedir  + camera + '.1hour-darks.stddev.v'+version+'.png'
WRITE_PNG, fname, TVRD(/TRUE)

stop

label_the_end : 
END