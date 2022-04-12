;; fitting function


function dark_noise_model, x, par

    return, par[0]*x + par[1]
end

function fit_dark_noise_model, exps_ms, dark_noise

    n = n_elements(dark_noise)
    errs = fltarr(n)
    errs[*] = 0.0001
    exps_s = exps_ms * 0.001
    print, exps_s
    parinit = [(dark_noise[1]-dark_noise[0])/(exps_s[1]-exps_s[0]),dark_noise[0]]
    ;parinit = [(dark_noise[1]-dark_noise[0])/(exps_s[1]-exps_s[0]),0]
    ;parinit = [0,0]

    pfit = mpfitfun('dark_noise_model',exps_s,dark_noise,errs,parinit,maxiter=100)

    return, pfit
end


;;---------------------------------------------------------------------------
;;---------------------------------------------------------------------------

;        ['white'  , 'green'  , 'red'    , 'yellow']
colors = ['ffffff'x, '66ff00'x, '9900ff'x, '00ffff'x] ; 'bbggrr'x
;        [◇        , △        , □        , x        ]
psyms  = [4        , 5        , 6        , 7        ]

date = '20220316'
identifier = 'ircamera-experiment'
savedir = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'+date+'.'+identifier+'/'
charsize=2


gotolabel = 1

case gotolabel of 
    1 : goto, label_data_and_plot
endcase

label_data_and_plot:

_IS_SAVE = 1b
_IS_FIT  = 1b

version = '0320'

cameras = ['gold','xeva','flir']

;hash_stdv = hash()

;;-- loop over cameras
for kk=0, n_elements(cameras)-1 do begin
camera = cameras[kk]

;if camera ne 'gold' then continue
;if camera ne 'flir' then continue

case camera of 
    'gold' : begin
        root = '/mnt/HDD3TBn52/DST/vs/20220320/'
        folder = root + camera + '/'
        exps = ['0004','0008','0016','0040','0100','0200','0500','1000']
    end
    'flir' : begin
        root = '/mnt/HDD3TBn52/DST/vs/20220320/'
        folder = root + camera + '/'
        exps = ['0001','0002','0004','0008','0015','0040','0100','0200','0500','1000']
    end
    else : begin
        root = '/mnt/HDD3TBn51/DST/vs/20220316/'
        folder = root + camera + '/'
        exps = ['0001','0002','0004','0008','0016','0040','0100','0200','0500','1000']
    end
endcase

print, 'processing folder : ', folder

nexp = n_elements(exps)
arr_mean = fltarr(nexp)
arr_maxi = fltarr(nexp)
arr_mini = fltarr(nexp)
arr_medi = fltarr(nexp)
arr_stdv = fltarr(nexp)
arr_exps = fltarr(nexp)
arr_mstd = fltarr(nexp)
arr_stdt = fltarr(nexp)
;;-- loop over exposure
for jj=0, n_elements(exps)-1 do begin
    exp = exps[jj]
    arr_exps[jj] = float(exp)

    ;if (exp eq '0008') and (camera eq 'gold') then begin
    if (camera eq 'gold') or (camera eq 'flir') then begin
        fdatas = findfile(folder+'/dark_e'+exp+'_*.fits')
        data = float(readfits(fdatas[0],hd))
        data = data[*,*,30:99]
    endif else begin
        fdatas = findfile(folder+'/exp1_'+exp+'_'+camera+'*.fits')
        data = float(readfits(fdatas[0],hd))
    endelse

    ;if camera eq 'gold' then print, 'exp=',exp, '  ', hd[17]

    ;; data (nx,ny,5)
    ss = size(data) & nd = ss[0] & nx=ss[1] & ny=ss[2] & nt = ss[3]
    ;print, 'nd=', nd, ' nt=', nt

    im  = mean(data, dimension=3)
    arr_mean[jj] = mean(im)
    mm = minmax(im)
    arr_mini[jj] = mm[0]
    arr_maxi[jj] = mm[1]
    arr_medi[jj] = median(im)

    ;arr_stdv[jj] = mean( sqrt( mean(data*data,dimension=3) ) )
    ;arr_stdv[jj] = mean( stddev(data, dimension=3) )
    
    arr_mstd[jj] = stddev( (mean(mean(data, dimension=1),dimension=1)) )

    ;; RSS
    im = 0
    for ii=0, nt-2 do begin
        ;im += (data[*,*,ii+1] - data[*,*,ii])^2
        im += total((data[*,*,ii]-data[*,*,ii+1])^2)/nx/ny
    endfor
    im /= (nt-1)
    ;im = sqrt(im)
    ;arr_stdv[jj] = mean(im)
    arr_stdv[jj] = im

    ;; RSS without bias
    im = 0
    for ii=0, nt-2 do begin
        ;im += (data[*,*,ii+1] - data[*,*,ii])^2
        im += total(( (data[*,*,ii]-mean(data[*,*,ii])) - (data[*,*,ii+1]-mean(data[*,*,ii+1])) )^2)/nx/ny
    endfor
    im /= (nt-1)
    ;im = sqrt(im)
    ;arr_stdv[jj] = mean(im)
    arr_stdt[jj] = im

    
    

endfor ;;-- end loop over exposure

fac = 1

if camera eq 'gold' then arr_exps -= 3.32

window, kk+3, xs=500, ys=500

plot, arr_exps*fac,arr_maxi,color=colors[0], /xlog,/ylog,xtitle='exposure [ms]',title=camera, charsize=charsize, yr=[1E2,1E5], xr=[1E-1, 1E3]
oplot, arr_exps*fac,arr_maxi,color=colors[0], psym=psyms[1],syms=1.5

oplot, arr_exps*fac,arr_mean,color=colors[1]
oplot, arr_exps*fac,arr_mean,color=colors[1], psym=psyms[1],syms=1.5

oplot, arr_exps*fac,arr_medi,color=colors[2]
oplot, arr_exps*fac,arr_medi,color=colors[2], psym=psyms[1],syms=1.5

oplot, arr_exps*fac,arr_mini,color=colors[3]
oplot, arr_exps*fac,arr_mini,color=colors[3], psym=psyms[1],syms=1.5

fname = savedir + '/dark/' + camera+'.darklevel.v'+version+'.png'
if _IS_SAVE then WRITE_PNG, fname, TVRD(/TRUE) & print, 'saved as : ', fname



window, kk+8, xs=500, ys=500


;print, arr_exps
;print, arr_stdv
;plot, arr_exps,arr_stdv,color=colors[0],psym=psyms[3], syms=1.5, /xlog,xtitle='exposure [ms]',ytitle='(DN^2)', title=camera, charsize=charsize, yr=[0,3000], xr=[1E-1, 1E4]

plot, arr_exps*fac,sqrt(arr_stdv),color=colors[0],psym=psyms[3], syms=1.5, /xlog,xtitle='exposure [ms]', title=camera, charsize=charsize, yr=[0,60], xr=[1E-1, 1E3]
if (camera eq 'gold') or (camera eq 'flir') then begin 
oplot, arr_exps*fac, arr_mstd, color=colors[0], psym=psyms[0], syms=1
oplot, arr_exps*fac, arr_mstd, color=colors[0]
endif

if (camera eq 'gold') then begin
oplot, arr_exps*fac,sqrt(arr_stdt),color=colors[1],psym=psyms[1], syms=1.5
endif


if _IS_FIT then begin
    mask = where(arr_exps le 500)
    pfit = fit_dark_noise_model(arr_exps[mask], arr_stdv[mask])
    print, 'pfit = ', pfit
    increment = 0.1
    ;; x in [s]
    x = 10^( findgen(fix(4.1/increment)+1,start=0,increment=increment) ) * 0.0001
    oplot, x*1000*fac, sqrt(dark_noise_model(x,pfit)), color=colors[2]
    print, 'readout noise = ', sqrt(pfit[1])
    print, 'thermal noise coefficient = ', sqrt(pfit[0])

    if (camera eq 'gold') then begin
    pfit = fit_dark_noise_model(arr_exps[mask], arr_stdt[mask] )
    oplot, x*1000*fac, sqrt(dark_noise_model(x,pfit)), color=colors[1]
    print, 'readout noise = ', sqrt(pfit[1])
    print, 'thermal noise coefficient = ', sqrt(pfit[0])
    endif

    fname = savedir + '/dark/' + camera + '.darknoise.v'+version+'.png'
    if _IS_SAVE then WRITE_PNG, fname, TVRD(/TRUE) & print, 'saved as : ', fname
endif


;stop
;;hs = hash()
;;hs['exps'] = arr_exps
;;hs['stdv'] = arr_stdv
;;hash_stdv[camera] = hs

endfor ;;-- end loop over cameras

;;wid_stdv = 17
;;window, wid_stdv, xs=800, ys=600
;;for kk=0, n_elements(cameras)-1 do begin
;;camera = cameras[kk]
;;
;;hs = hash_stdv[camera]
;;arr_exps = hs['exps']
;;arr_stdv = hs['stdv']
;;
;;if kk eq 0 then begin
;;    plot, arr_exps,arr_stdv,color=colors[kk], /xlog,xtitle='exposure [ms]',ytitle='(DN)', charsize=charsize, yr=[0,200], xr=[1E0, 1E3]
;;    oplot, arr_exps,arr_stdv,color=colors[kk], psym=psyms[3], syms=1.5
;;endif else begin
;;    oplot, arr_exps,arr_stdv,color=colors[kk]
;;    oplot, arr_exps,arr_stdv,color=colors[kk], psym=psyms[3], syms=1.5
;;endelse
;;
;;
;;if kk eq n_elements(cameras)-1 then begin
;;    fname = savedir + '/dark/' + 'cameras.darknoise.v'+version+'.png'
;;    if _IS_SAVE then WRITE_PNG, fname, TVRD(/TRUE)
;;endif
;;
;;endfor ;;-- end loop over cameras



stop

END



