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

_DATAROOT = '/mnt/HDD3TBn53/DST/sp/20220409/'
_MY_WORKDIR = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'
_MY_FOLDER  = '20220409.prominence-experiment/'
_IMG_FMT = 'png'  ; 'png', 'gif'
_path_pcal_init = ''
_force_pcal_init = 0b
_check_polarizer = 0b
;---------------------------------------------------------------------------
obs = 'He.fl.2.3'
;dfobs = 'He.fl.5.1'
;obs = 'He.ar.1.1'
;obs = 'He.pr.1.3'
;obs = 'cir.20220405'
;obs = 'cir'
case obs of
;;----[He.fl.2.3]----------------------------------------
    'He.fl.2.3': begin
	_wave0 = '10830' & _exp0 = '15' & _rotp0 = '10' & _setname = 'fl.2.3'
	path.rootdir = 	_DATAROOT + 'spec/He_I_10830/' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp0+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp0+'_r'+_rotp0+'_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e'+_exp0+'.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = path.workdir + path.outdir + _wave0+'.pcal.sav'	; polari. calib params.
	;cal.pcal = path.workdir + path.outdir + 	_wave0+'.pcal.sav'	; polari. calib params.

	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 120 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 80 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 39.60		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	check_polarizer = 0b
        _force_pcal_init = 1b
        _path_pcal_init = path.workdir + path.outdir + '/../../20220420.xclass-ar/ar.2.1/' + _wave0 + '.pcal.sav'
	my_i2quv = [-0.015, 0.016, -0.0015]  
	pmax0 = 0.002
	end
;;----[He.fl.5.1]----------------------------------------
	'He.fl.5.1': begin
	_wave0 = '10830' & _exp0 = '15' & _rotp0 = '10' & _setname = 'fl.5.1'
	path.rootdir = 	_DATAROOT + 'spec/He_I_10830/' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp0+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp0+'_r'+_rotp0+'_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e'+_exp0+'.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = path.workdir + path.outdir + '../../share/' +	_wave0+'.pcal.WEST.20220420.ar.2.1.sav'	; polari. calib params.
	cal.pcal = path.workdir + path.outdir + '../fl.2.3/' +	_wave0+'.pcal.sav'	; polari. calib params.

	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 100 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 50 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 326.07		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = -1		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	my_i2quv = [-0.0115,-0.001,0.003]  
	pmax0 = 0.02 
	end
;;----[He.pr.1.3]----------------------------------------
	'He.pr.1.3': begin
	_wave0 = '10830' & _exp0 = '50' & _rotp0 = '10' & _setname = 'pr.1.3'
	path.rootdir = 	_DATAROOT + 'spec/He_I_10830/' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp0+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp0+'_r'+_rotp0+'_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e'+_exp0+'.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = path.workdir + path.outdir + '../../share/' +	_wave0+'.pcal.WEST.20220420.ar.2.1.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.telpos="WEST"
	dinfo.nstep = 50 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 25 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.incli = 5.567	 	; inclination if dst_info not available
	;dinfo.th_offset = 33.53		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = ''			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 1			; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = -1		; 0 - no correction, 1 - use i2quv[2,3],  2 - correct with own IQUV 

	my_i2quv = [-0.005, 0.028, 0.]  
	my_i2quv = [0.00, 0.00, 0.]  
	pmax0 = 0.005
	end
   


   'He.pr.1.5': begin
   _wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn53/DST/sp/22020409/spec/He_I_10830/'	; root directory of raw dataset
	path.obsdir = 'pr.1.5/'				; holder of obs dataset
	path.obsdat = '*.fits'		; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e50_r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e50_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e50_r10_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER			; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e50.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = calpardir+	_wave0+'.pcal.WEST.20220405.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.telpos="WEST"
	dinfo.nstep = 25 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 20 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.incli = 5.567	 	; inclination if dst_info not available
	;dinfo.th_offset = 33.53		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = ''			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0			; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = -1		; 0 - no correction, 1 - use i2quv[2,3],  2 - correct with own IQUV 

	my_i2quv = [-0.03, -0.001, 0.]
	end

   'He.pr.3.3': begin
	path.rootdir = '/mnt/HDD3TBn53/DST/sp/22020409/spec/He_I_10830/'	; root directory of raw dataset
	path.obsdir = 'pr.3.3/'				; holder of obs dataset
	path.obsdat = '*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e30_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e30_r10_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER			; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	'dark_e30.sav'	
	cal.flat = calpardir+	'flat_He.sav'
	cal.ap = calpardir+	'ap_He.sav'
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'
	cal.pcal = calpardir+	'pcal.10830.20220405.EAST.sav'
	cal.wl = calpardir+	'wl_He.sav'

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 50 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 25 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.incli = 5.90	 	; inclination if dst_info not available
	;dinfo.th_offset = 32.75		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = ''			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 1			; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 0		; 0 - no correction, 1 - use i2quv[2,3],  2 - correct with own IQUV 

	my_i2quv = [0.0223, 0., 0.]
	end

   'He.fl.2.3.origin': begin
   _wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn53/DST/sp/20220409/spec/He_I_10830/'	; root directory of raw dataset
	path.obsdir = 'fl.2.3/'				; holder of obs dataset
	path.obsdat = '*.fits'		; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e50_r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e15_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e15_r10_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER			; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e15.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = calpardir+	_wave0+'.pcal.WEST.20220405.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	;dinfo.telpos="EAST"
	dinfo.telpos="WEST"
	dinfo.nstep = 120 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 60 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.incli = 39.60	 	; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0			; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use i2quv[2,3],  2 - correct with own IQUV 

	;my_i2quv = [-0.02, 0.008, 0.]   ; EAST with WEST pcal
	my_i2quv = [-0.0075, 0., 0.]   ; WEST with WEST pcal
	pmax0 = 0.005
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

