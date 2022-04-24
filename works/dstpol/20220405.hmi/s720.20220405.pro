
path = '/nwork/kouui/data-lvl1/dstpol/20220405.hmi/fits/s720/converted/'

identifiers = ['I','Q','U','V']
for i=0, n_elements(identifiers)-1 do begin
    char = identifiers[i]
    files = findfile(path + '*3.'+char+'*.fits')
;    print, 'found files for ', char
;    print, files
    img = readfits(files[0], hd, exten_no=0)
    stop
endfor
;nwave = 6

;window, 0, xs=256, ys=256
;tvscl, img[1000:1000+256,2300:2300+256]

END