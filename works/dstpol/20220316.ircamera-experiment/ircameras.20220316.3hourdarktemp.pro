
function hd2temp, hd
    s = hd[26]
    words = strsplit(hd[26], '=', /extract)
    return, float(words[1])
end

;;------------------------------------------------------------------
;;------------------------------------------------------------------


;        ['white'  , 'green'  , 'red'    , 'yellow']
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x

date = '20220316'
identifier = 'ircamera-experiment'
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'+date+'.'+identifier+'/'
savedir += '3-hour-dark-temperature/'
charsize=1.5

gotolabel = 2

case gotolabel of 
    1 : goto, label_make_data
    2 : goto, label_visual1
endcase
goto, label_the_end

;;------------------------------------------------------------------
;;------------------------------------------------------------------

label_make_data : 

_IS_SAVE = 1b
version = '0319'
version = '0319.+00C2h'
version = '0319.+10C10h'
version = '0320.temptest.manual'

camera = 'gold'

case version of 
    '0318' : begin
        root = '/mnt/HDD3TBn51/DST/vs/'+'20220318'+'/'
        folder = root + camera + '.3hourdark.temperature/'
        temps = ['+05C', '+00C', '-05C'];,'-10C']
    end
    '0319' : begin
        root = '/mnt/HDD3TBn52/DST/vs/'+'20220319'+'/'
        folder = root + camera + '.1hourdark.obs/'
        temps = ['+05C', '+10C']
    end
    '0319.+00C2h' : begin
        root = '/mnt/HDD3TBn52/DST/vs/'+'20220319'+'/'
        folder = root + camera + '.2hourdark/'
        temps = ['+00C']
    end
    '0319.+10C10h' : begin
        root = '/mnt/HDD3TBn52/DST/vs/'+'20220319'+'/'
        folder = root + camera + '.1hourdark.obs/'
        temps = ['+10C.3']
    end
    '0320.temptest.manual' : begin
        root = '/mnt/HDD3TBn52/DST/vs/'+'20220320'+'/'
        folder = root + camera + '.temptest/'
        temps = ['+05C', '+10C']
    end
endcase
print, 'processing folder : ', folder



;;-- loop over temperature
for jj=0, n_elements(temps)-1 do begin
temp = temps[jj]
print, 'processing data with temperature = ', temp

fdatas = findfile(folder+temp+'/*.fits')
print, 'found ', n_elements(fdatas), ' data files' 
nf =  n_elements(fdatas)
img = readfits(fdatas[0],hd, /silent)
ss = size(img) & nx = ss[1] & ny = ss[2]


zero_arr = ulonarr(nf)
hots_arr = ulonarr(nf)
mean_arr = fltarr(nf)
stdv_arr = fltarr(nf)
temp_arr = fltarr(nf)

;;-- loop over file
for i=0, nf-1 do begin
    print, 'processing file no.', i, '/', nf-1
    case version of 
        '0319' : img = float(readfits(fdatas[i],hd, /silent, nslice=0))
        '0319.+10C10h' : img = float(readfits(fdatas[i],hd, /silent, nslice=0))
        '0320.temptest.manual' : img = float(readfits(fdatas[i],hd, /silent, nslice=0))
        else : img = float(readfits(fdatas[i],hd, /silent))
    endcase
    mean_arr[i] = mean(img)
    if i gt 0 then stdv_arr[i] = stddev(img-imgprev) 
    temp_arr[i] = hd2temp(hd)

    ;; find zeros values
    pos = where(img eq 0, count)
    zero_arr[i] = count

    ;; find hot pixels
    pos = where(img gt 4*median(img))
    if total(pos) ge 0 then hots_arr[i] = n_elements(pos)
    imgprev = img
endfor ;;-- end loop over file
stdv_arr[0] = stdv_arr[1]

if 1b then begin
res = hash()
res['mean_arr'] = mean_arr
res['stdv_arr'] = stdv_arr
res['temp_arr'] = temp_arr
res['zero_arr'] = zero_arr
res['hots_arr'] = hots_arr
fname = savedir+camera+'.3hdt.'+temp+'.v'+version+'.sav'
if _IS_SAVE then save, res, filename=fname & print, 'saved as : ', fname
endif

endfor ;;-- end loop over temperature


stop

;;------------------------------------------------------------------
;;------------------------------------------------------------------

label_visual1:

_IS_SAVE = 1b
version = '0319.+00C2h'
version = '0319.+10C10h'
version = '0320.manual.+05C'
;version = '0320.manual.+10C'
version = '0320.temptest.manual'
camera = 'gold'

case version of 
    '0318' : begin
        temps = ['+05C', '+00C', '-05C'];,'-10C']
        frate = 0.5
    end
    '0319' : begin
        temps = ['+05C','+10C']
        frate = 0.25
    end
    '0319.+00C2h' : begin
        temps = ['+00C']
        frate = 1
    end
    '0319.+10C10h' : begin
        temps = ['+10C.3']
        frate = 0.25
    end
    '0320.temptest.manual' : begin
        temps = ['+05C', '+10C']
        frate = 0.25
    end
endcase


;;-- loop over temperature
;!p.multi=[0,1,1]
wid = 5
undefine, xtitle
titles = ['zero counting','hot pixel counting', 'sensor temperature', 'mean of image', 'stddev of image']
ytitles = ['count', 'count', 'temperature [C]', 'DN', 'DN']
;xtitles = ['', '', '', '', 'time [sec]']
;xtitles = ['time [sec]', 'time [sec]', 'time [sec]', 'time [sec]', 'time [sec]']
names = ['zero_arr', 'hots_arr', 'temp_arr', 'mean_arr', 'stdv_arr']
;window, 7, xs=800, ys=1600
;;-- loop over plots
for k=0, 5-1 do begin 
name = names[k]
if name eq 'hots_arr' then continue
;if name ne 'stdv_arr' then continue
;window, 5+k, xs=300, ys=400
window, 5+k, xs=800, ys=400
;;-- loop over temperature
for jj=0, n_elements(temps)-1 do begin
    temp = temps[jj]
    fname = savedir+camera+'.3hdt.'+temp+'.v'+version+'.sav'
    restore, fname
    ;print, 'restored : ', fname
    data = res[name]
    ;;if name eq 'temp_arr' then stop
    ;;if jj eq 0 then print, name, '[0] = ', data[0]

    xtitle = 'time [hour]'
    x = (indgen(n_elements(data))+1) / 3600. / frate
    ;if version eq '0319.+00C2h' then $
    ;x = (indgen(n_elements(data))+1)  / frate & $
    ;xtitle = 'time [sec]'


    case name of 
        'temp_arr' : yr = [0,15];[-15,15]
        'hots_arr' : yr = [-1,1]
        'zero_arr' : yr = [-1,1]
        'stdv_arr' : yr = [25,30];[25,30]
        'mean_arr' : yr = [1200,1500];[1000,2500];[1000,1500]
    endcase

    
    if jj eq 0 then begin 
    ;plot, x, data, color=colors[jj],yr=yr, charsize=charsize, xtitle=xtitle, ytitle=ytitles[k], title=titles[k],psym=2,symsize=0.6, xr=[1140,1180]
    ;oplot, x, data, color=colors[jj]
    plot, x, data, color=colors[jj],yr=yr, charsize=charsize, xtitle=xtitle, ytitle=ytitles[k], title=titles[k]
    endif else oplot, x, data,color=colors[jj]

endfor ;;-- end loop over temperature

;fname = savedir+'/png/'+camera+'.3hdt.'+name+'.v'+version+'.enlarge.png'
fname = savedir+'/png/'+camera+'.3hdt.'+name+'.v'+version+'.png'
if _IS_SAVE then WRITE_PNG, fname, TVRD(/TRUE)

endfor ;;-- end loop plots



label_the_end : 
END