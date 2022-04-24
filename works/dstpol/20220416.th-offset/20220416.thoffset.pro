@my_colors

exps = [[60., 15., 100.,40.],[50., 15., 30., 100.]]   ; [msec]
rots = [[1.5, 1.0, 2.0, 1.5],[1.0, 1.0, 1.0, 2.0]]   ; [sec]
tofs = [[30.90, 26.50, 32.75, 28.535],[33.53, 27.25, 29.95, 33.80]]  ;[deg]
tofus = [[31.078, 26.424, 32.952, 28.677],[32.643, 26.37, 29.082, 32.908]]  ;[deg]
;tofsp= [31.50, 26.50, 33.20, 33.53]  ;[deg]

deadtime = 3.32
x = (0.5 * (exps - deadtime) * 0.001) * 360. / rots
;y = tofs
;yf = x + 23.8

window, 11, xs=1000, ys=500, title="deadtime = "+string(deadtime,form='(f6.3)')+' [ms]'

;for i=0,1 do begin
;if i eq 0 then plot, x[*,i], tofs[*,i], psym=2, symsize=2, color=_colors[i], yr=[25,35], xtitle="(0.5 * exposure[sec]) * 360. / rots[sec],  [deg]", ytitle="th_offset,  [deg]" $
;else oplot, x[*,i], tofs[*,i], psym=2, symsize=2, color=_colors[i]
;endfor 

for i=0,1 do begin
if i eq 0 then plot, x[*,i], tofus[*,i], psym=2, symsize=2, color=_colors[i], charsize=2, yr=[25,35], xtitle= " 0.5 * (exposure[sec]-"+string(deadtime,form='(f6.3)')+"*0.001) * 360. / rotp[sec],[deg] ", ytitle="th_offset,  [deg]" $
else oplot, x[*,i], tofus[*,i], psym=2, symsize=2, color=_colors[i]
endfor

bias = [24.3]
for i=0,n_elements(bias)-1 do oplot, x[*,i], x[*,i]+bias[i],color=_colors[i]

;for i=0,1 do oplot, x[*,i], tofus[*,i], psym=4, symsize=2, color=_colors[i]

END