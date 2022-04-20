; dstpol_20220405_He.pro
@mmdst
@dst_pollib
@mmdst_lib
@quv_symfit
@undefine
@my_dst_pollib  ; overwrite functions in dst_pollib.pro

path = path_st()
dinfo = dinfo_st()
cal = calfile_st()

_MY_WORKDIR = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'
_MY_FOLDER  = '20220419.xclass-ar/'
_IMG_FMT = 'png'  ; 'png', 'gif'
;---------------------------------------------------------------------------
;; quv_symfit pcal inconsistency issue
;; IDL> restore, "10830.pcal.WEST.ar.1.6.sav"
;; IDL> pcal.pars
;; {
;;     "TH_VS": 0.0000000,
;;     "SC": 0.0000000,
;;     "XN": 0.019754909,
;;     "TN": 0.13952132,
;;     "XC": 0.017971702,
;;     "TC": 0.0034908073
;; }
;; IDL> restore, "10830.pcal.WEST.sav"
;; IDL> pcal.pars
;; {
;;     "TH_VS": 0.0000000,
;;     "SC": 0.0000000,
;;     "XN": 0.0058001671,
;;     "TN": 0.24566998,
;;     "XC": 0.00057072152,
;;     "TC": -0.22621842
;; }

;; ar.1.6, ar.1.7 : post eruption, surge, good seeing?
;; ar.1.26, ar.1.27, ar.1.28, ar.1.30 : post eruption, red shift?
; ar.1.11 : red shift?
; ar.1.17 : seeing not bad?
; 
obs = 'He.ar.1.19'
obs = 'He.ar.1.21'
obs = 'He.ar.1.6'
;obs = 'view'
case obs of
	'view': begin
	_wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn52/DST/sp/20220419/spec/He_I_10830/'	; root directory of raw dataset
	path.obsdir = 'ar.1.30/'				; holder of obs dataset
	path.obsdat = '*.fits'
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir
	calpardir = path.workdir+path.calpardir
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	dinfo.nstep = -1
	end
    'cir': begin
	_exp = '50' & _rot = '10'
	_wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn53/DST/sp/20220409/spec/He_I_10830/'	; root directory of raw dataset
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = '*.fits'				; observation seq. data
	path.obsdir = 'calib/'				; holder of obs dataset
	path.obsdat = 'cir_e'+_exp+'_r'+_rot+'*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp+'_r'+_rot+'*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = "cir/"				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e'+_exp+'.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	;cal.pcal = calpardir+	_wave0+'.pcal.WEST.20220405.sav'	; polari. calib params.
	;cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 3 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 1 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 331.80		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	pmax0 = 0.005 
	end
	'He.ar.1.6': begin
	_wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn52/DST/sp/20220419/spec/He_I_10830/'	; root directory of raw dataset
	path.obsdir = 'ar.1.6/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e15r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e20_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e20_r10_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e20.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = calpardir+	_wave0+'.pcal.WEST.ar.1.6.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 120 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 10 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 26.50		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	my_i2quv = [0, 0., 0]   ; WEST with WEST pcal
	pmax0 = 0.005 
	end
   'He.ar.1.19': begin
	_wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn52/DST/sp/20220419/spec/He_I_10830/'	; root directory of raw dataset
	path.obsdir = 'ar.1.19/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e15r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e15_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e15_r10_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e15.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = calpardir+	_wave0+'.pcal.WEST.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 80 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 65 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 9.90		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	my_i2quv = [0, 0., 0]   ; WEST with WEST pcal
	pmax0 = 0.02 
	end
   'He.ar.1.21': begin
	_wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn52/DST/sp/20220419/spec/He_I_10830/'	; root directory of raw dataset
	path.obsdir = 'ar.1.21/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e15r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e15_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e15_r10_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e15.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = calpardir+	_wave0+'.pcal.WEST.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 80 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 60 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 38.717		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	pmax0 = 0.02 
	end

	'He.pr.1.6': begin
	_wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn52/DST/sp/20220419/spec/He_I_10830/'	; root directory of raw dataset
	;path.obsdir = 'pr.1.6/'				; holder of obs dataset
	;path.obsdat = '*.fits'				; observation seq. data
	path.obsdir = 'calib/'				; holder of obs dataset
	path.obsdat = 'spot_e60_r15*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e60_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e60_r15_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e60.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = calpardir+	_wave0+'.pcal.WEST.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 50 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 25 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 30.88		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = ''			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 1		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	my_i2quv = [0., 0., 0.]
	pmax0 = 0.001 
	end

   
endcase

@dstpolcal_body

stop

disp:

; pr1 special correction
s1 = correct_DSTpol(s0, dinfo.wl0, dst, sc=pcal.pars.sc, mm=mm, pars=pcal.pars)
s2 = correct_Icrosstk(s1,yr=[280,320],correct='Q')
s2 = correct_Icrosstk(s2,yr=[100,320],correct='U')
dispiquvr,s2,pmax=pmax,bin=1,wid=2,/ialog,coms=['I','Q','U','V']





end

