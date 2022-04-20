
function Posbyte2long, barr
    ss = size(barr)
    if ss[0] ne 2 then throw_error,"barr must have dimension 2"
    if ss[1] ne 8 then throw_error,"barr must have nx=8"
    ny = ss[2]
    posl = lonarr(2,ny)
    for i=0,ny-1 do begin
        posl[0,i] = long(barr[3,i])
        posl[0,i]+= long(barr[2,i])*256l
        posl[0,i]+= long(barr[1,i])*256l*256l
        posl[0,i]+= long(barr[0,i])*256l*256l*256l
        posl[1,i] = long(barr[7,i])
        posl[1,i]+= long(barr[6,i])*256l
        posl[1,i]+= long(barr[5,i])*256l*256l
        posl[1,i]+= long(barr[4,i])*256l*256l*256l
    endfor
    return, posl
end

path = '/nwork/kouui/data-lvl1/dstpol/20220405.hmi/fits/me720/'

identifiers = ['inclination','azimuth','field']
for i=0, n_elements(identifiers)-1 do begin
    char = identifiers[i]
    files = findfile(path + '*.'+char+'.fits')
    print, 'found files for ', char
    print, files
    img = readfits(files[0], hd, /fpack)
    ;img = rice(files[0],2,hd)
    ;pos = Posbyte2long(pos)
    stop
endfor
;nwave = 6

END