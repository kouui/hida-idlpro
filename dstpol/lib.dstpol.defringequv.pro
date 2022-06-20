@lib.dstpol.mmdst
@lib.dstpol.mmdst_lib
@lib.dstpol.ichimoto.pro


function cos_, x, am, peri, ph
    return, am * cos( x*(2*!pi)/peri + ph )
end

function model_cos_, x, par
    return, par[0] * cos( x*(2*!pi)/par[1] + par[2] )
end

pro fringe_cos_, X, A, F
    am = A[0]
    peri = A[1]
    ph = A[2]
    F = cos_(X,am,peri,ph)

end

function frin1d2amph_fft_, arr1d, nfreq

    farr1d = fft(arr1d, -1)
    f = farr1d[nfreq]
    amp = sqrt( float(f*conj(f)) )
    ph  = atan( real_part(f) / IMAGINARY(f))
    return, [2*amp, ph+!pi]   ; phase deviated 1px
end

function frin1d2amph_mpfit_, x, arr1d, parinit
    err = 1E-5
    ret = mpfitfun('model_cos_',x,arr1d,err,parinit,/quiet)
    return,ret
end

function find_max_poly2d_, x, y, dd=dd
    if not keyword_set(dd) then dd=10
    ny = n_elements(y)
    val = max(y, pos)
    xi = (pos-dd > 0)
    xe = (pos+dd < (ny-1))
    xx = x[xi:xe]
    yy = y[xi:xe]
    c = poly_fit(xx,yy,2)
    return, c[1] / (-2.0 * c[2])
end

function conti2fringe_, conti, nfreq, xr

    ss = size(conti)
    if ss[0] ne 2 then THROW_ERROR,"required conti as 2d array"
    nx = ss[1] & ny = ss[2]
    fconti = complexarr(nx,ny)
    for i=0,nx-1 do fconti[i,*] = fft( conti[i,*], -1 )
    avg = mean(fconti[xr[0]:xr[1],nfreq])
    fconti_flat = fconti
    fconti_flat[*,nfreq] = avg
    fconti_flat[*,ny-nfreq] = avg
    
    conti_frin = fltarr(nx,ny)
    for i=0,nx-1 do conti_frin[i,*] = fft(  fconti[i,*]-fconti_flat[i,*] ,1)

    return, conti_frin

end


pro calibrate_defringequv_, savdir, bin, pmax, n_conti_fringe=n_conti_fringe, divi=divi, $
    pyc=pyc, period=period, peri0=peri0, xrflat=xrflat, isreverse=isreverse, ix=ix

_libname_ = "[DeFringeQUV] "
if not keyword_set(bin) then bin = 1
if not keyword_set(pmax) then pmax = 0.002

savfiles = findfile( savdir + '/*.sav' )
if savfiles[0] eq '' then throw_error, _libname_ + '0 *.sav file found in '+savdir
nsav = n_elements(savfiles)
text = _libname_ + 'total ' + strtrim(nsav, 2) + ' *.sav found in '+savdir
print, text

;; if not subpix period given, then calculate it
;if not keyword_set(period) then begin

;; step 1. select a dataset with small quv signal
while 1b do begin
    isfound = 0b
    for i=0, nsav-1 do begin
        restore, savfiles[i];, /verbose
        UNDEFINE,pcal, dinfo, h, xprof, dx   ; undefine unecessary variables
        dispiquvr,s[*,*,0:3],bin=bin,pmax=pmax,wid=10
        if keyword_set(divi) then for j=1,3 do s[*,*,j] /= s[*,*,0]
        text = _libname_ + 'use set No.'+strtrim(i,2)+' as example?'
        ans = wdyesno(text,x=400,y=300)
        if ans then begin
            isfound = 1b
            break
        endif
        if (((i+1) mod 5) eq 0) then begin
            ans=wdyesno("whether to reset?",x=400,y=300)
            if ans then break
        endif
    endfor
    if isfound then break
endwhile
wdelete, 10
;; step 2. select x position to perform calculation
;wdok, 'select xpos to calculate fringe period'
if not keyword_set(ix) then $
while 1b do begin
    ix = 130
    dispiquvr,s[*,*,0:3],bin=bin,pmax=pmax,get_ipos=ix, wid=10
    text = _libname_ + 'selected x = '+strtrim(ix,2)
    print, text
    window, 11, xs=800, ys=600
    plot, s[ix,*,1], title='Q profile at x='+strtrim(ix,2)
    ans = wdyesno("select another x position?",x=400,y=300)
    if not ans then break
endwhile 

iq = 1

;; step 3. click continuum(far wing) and the direction to perform fft
window, 11, xs=800, ys=600
plot, s[ix,*,iq], title='Q profile at x='+strtrim(ix,2)
print, _libname_ + 'click to select a continuum position'
cursor,pxc,pyc,/data
wait, 0.5
UNDEFINE,pyc
print, _libname_ + 'click to select the direction to perform fft'
cursor,pxd,pyd,/data
UNDEFINE,pyd
if pxc gt pxd then isreverse = 1b else isreverse = 0b
print, _libname_ + 'whether to perform fft in reverse wavelength direction? --> ', isreverse

ss = size(s) & nx = ss[1] & ny = ss[2]; && ns = ss[3]

if isreverse then begin
    s = reverse(s,2)
    pyc = ny-1-pxc
endif else pyc = pxc
UNDEFINE,pxc, pxd

;; step 4. click box range to select a x range where fringe is insignificant
xrflat = [200,250]

;; step 5. find fringe period
prof = reform(s[ix,0:ny-1,iq])
fprof = fft(prof,-1)
np = n_elements(prof)
window, 11, xs=800, ys=600
amp_fprof = fprof*conj(fprof)
plot, amp_fprof[0:np/2], title='fft spectrum of Q profile at x='+strtrim(ix,2);, /ylog;, psym=3, symsize=2
freq = 10
ans = wdgetstr("modify fringe frequency = "+strtrim(freq,2)+" ? (enter to skip)",xpos=500,ypos=300)
if ans ne '' then freq = fix(ans)

ntest = 2*np/freq
amp_freq   = fltarr(ntest)
amp_freqm1 = fltarr(ntest)
for i=0,ntest-1 do begin
    ymax = np - 1 - i
    prof = reform(s[ix,0:ymax,iq])
    fprof = fft(prof,-1)
    amp_fprof = fprof*conj(fprof)
    amp_freq[i] = amp_fprof[freq]
    amp_freqm1[i] = amp_fprof[freq-1]
endfor
;stop
window, 11, xs=800, ys=600
x = indgen(ntest)
plot, x,amp_freq, xtitle="ny-i",title='cut wavepength profile to find fringe period'
oplot, x,amp_freqm1, line=-1

period0 = find_max_poly2d_(x,amp_freqm1) - find_max_poly2d_(x,amp_freq)
period0 = fix(period0)
peri0 = period0
print, _libname_ + 'found period (non subpix) = ', strtrim(period0)


;; step 6. calculate continuum fringe (subimage)
if not keyword_set(n_conti_fringe) then n_conti_fringe = 2

conti_frin = conti2fringe_(s[*,pyc:pyc+n_conti_fringe*period0-1,iq], n_conti_fringe, xrflat)
;print, size(conti_frin)
window, 12, xs=nx,ys=n_conti_fringe*period0
tvscl, conti_frin

prof = reform(conti_frin[ix,*])
x = indgen(n_conti_fringe*period0)
A = [max(prof),period0,0.]
;; cannot fit correctly
;yfit = CURVEFIT(X, prof, weights, A, SIGMA, FUNCTION_NAME='fringe_cos_', /NODERIVATIVE)
ret = frin1d2amph_mpfit_(x, prof, A)
if ret[0] lt 0 then begin ;; fix minus amplitude
    ret[0] *= -1
    ret[2] += !pi
endif

;; step 7. maximize cross correlation to find subpix period
ntest = 101
peris = findgen(ntest) * 0.04 - 2 + period0
ccorrs= fltarr(ntest)
x = findgen(ny-pyc)
for i=0, ntest-1 do begin
    prof = reform(s[ix,pyc:ny-1,iq])
    ret[1] = peris[i]
    frin =  model_cos_(x, ret)
    ccorrs[i] = total( prof*frin )
endfor

period = find_max_poly2d_(peris, ccorrs, dd=20)
print, _libname_ + 'found period (subpix) = ', strtrim(period)

window, 13, xs=800, ys=600
plot, peris, ccorrs, xtitle='fringe period', title='cross correlation with original profile'

end

pro defringe_s2d_, quv2d, n_conti_fringe,pyc, period, peri0, xrflat, isreverse, fringe=fringe, out=out
    
    ss = size(quv2d)
    if ss[0] ne 2 then THROW_ERROR,"required quv2d as 2d array"
    nx = ss[1]
    ny = ss[2]
    period0 = peri0
    if isreverse then arr = reverse(quv2d,2) $
    else arr = quv2d
    
    conti_frin = conti2fringe_(arr[*,pyc:pyc+n_conti_fringe*period0-1], n_conti_fringe, xrflat)
    x = indgen(n_conti_fringe*period0)
    xx = indgen(ny) - pyc
    fringe = fltarr(nx,ny)
    for i=0,nx-1 do begin
        prof = conti_frin[i,*]
        A = [max(prof),period0,0.]
        ret = frin1d2amph_mpfit_(x, prof, A)
        if ret[0] lt 0 then begin ;; fix minus amplitude
            ret[0] *= -1
            ret[2] += !pi
        endif
        ret[1] = period
        frin1d = model_cos_(xx, ret)
        fringe[i,*] = frin1d
    endfor
    out = arr-fringe
    if isreverse then begin
        out = reverse(out,2)
        fringe = reverse(fringe,2)
    endif
end
;; 
n_conti_fringe = 2
ix = 130
savdir = '/nwork/ichimoto/DST/sp/20220529_He/df.1.6/'
bin = 1
pmax = 0.002
calibrate_defringequv_, savdir, bin, pmax, ix=ix, $
    n_conti_fringe=n_conti_fringe, pyc=pyc, period=period, peri0=peri0, $
    xrflat=xrflat, isreverse=isreverse
savfiles = findfile( savdir + '/*.sav' )
for jj =0, n_elements(savfiles)-1 do begin
    restore, savfiles[jj];, /verbose
    s = s[*,*,0:3]
    for j=1,3 do s[*,*,j] /= s[*,*,0]
    s = contizero_s3d(s)
    UNDEFINE,pcal, dinfo, h, xprof, dx   ; undefine unecessary variables
    ;for i=1,3 do s[*,*,i] /= s[*,*,0]
    ss = size(s)
    nx = ss[1]
    ny = ss[2]
    s3d    = fltarr(nx,ny,4)
    s3d[*,*,0] = s[*,*,0]
    frin3d = fltarr(nx,ny,3)
    for i=1,3 do begin
        defringe_s2d_,reform(s[*,*,i]), n_conti_fringe,pyc, period, peri0, xrflat, isreverse, fringe=fringe, out=out
        s3d[*,*,i] = out
        frin3d[*,*,i-1] = fringe
    endfor

    dispiquvr,s,bin=bin,pmax=pmax,wid=15
    dispiquvr,s3d,bin=bin,pmax=pmax,wid=16
endfor

END



