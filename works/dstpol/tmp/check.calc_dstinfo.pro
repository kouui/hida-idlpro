
@calc_dstinfo

times = ['2022-01-22T14:54:52.000','2022-01-22T15:24:53.000','2022-01-22T15:57:38.000']
inclis = [0.03,0.03,0.10]

ans_ha = [[2,49,42],[3,21,44],[3,54,30]]
ans_zd = [[-68,50,10],[-73,28,5],[-78,39,25]]
ans_az = [[42,54,14],[49,10,25],[55,1,34]]

n = n_elements(times)

for i=0, n-1 do begin
	dst = calc_dstinfo(times[i],inclis[i])
   print,'----------------------------------------'
	print, times[i]
	print, "HA cal : ", dst[*,0]
   print, "HA log : ", ans_ha[*,i]
   print, "ZD cal : ", dst[*,1]
   print, "ZD log : ", ans_zd[*,i]
   print, "AZ cal : ", dst[*,9]
   print, "AZ log : ", ans_AZ[*,i]
endfor

END

;; errors
;; HA : 1-3 [min] -> 0.05 [deg]
;; ZD : 30-50 [min] -> 0.80 [deg]
;; AZ : -1-30 [min] -> 0.50 [deg]

;;IDL> .r check.calc_dstinfo.pro
;;% Compiled module: VAL2ANG.
;;% Compiled module: CALC_DSTINFO.
;;% Compiled module: $MAIN$.
;;----------------------------------------
;;2022-01-22T14:54:52.000
;;HA cal :        2      53       1
;;HA log :        2      49      42
;;ZD cal :       69      35      28
;;ZD log :      -68      50      10
;;AZ cal :       43      22      26
;;AZ log :       42      54      14
;;----------------------------------------
;;2022-01-22T15:24:53.000
;;HA cal :        3      23       2
;;HA log :        3      21      44
;;ZD cal :       73      57      59
;;ZD log :      -73      28       5
;;AZ cal :       49      11      35
;;AZ log :       49      10      25
;;----------------------------------------
;;2022-01-22T15:57:38.000
;;HA cal :        3      55      47
;;HA log :        3      54      30
;;ZD cal :       79      10      39
;;ZD log :      -78      39      25
;;AZ cal :       55       0      32
;;AZ log :       55       1      34
