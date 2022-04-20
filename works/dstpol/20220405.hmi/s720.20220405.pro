
path = '/nwork/kouui/data-lvl1/dstpol/20220405.hmi/fits/s720/'

identifiers = ['I','Q','U','V']
for i=0, n_elements(identifiers)-1 do begin
    char = identifiers[i]
    files = findfile(path + '*3.'+char+'*.fits')
;    print, 'found files for ', char
;    print, files
    img = readfits(files[0], hd, hp, exten_no=1)
    stop
endfor
;nwave = 6

END