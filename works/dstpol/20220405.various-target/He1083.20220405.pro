@mmdst
@dst_pollib
@mmdst_lib
@quv_symfit
@undefine
@my_dst_pollib  ; overwrite functions in dst_pollib.pro

path = path_st()
dinfo = dinfo_st()
cal = calfile_st()

_DATAROOT = '/mnt/HDD3TBn53/DST/sp/20220405/'
_MY_WORKDIR = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'
_MY_FOLDER  = '20220405.various-target/'
_IMG_FMT = 'png'  ; 'png', 'gif'
;---------------------------------------------------------------------------
; 

obs = 'He.ar.1.1'
obs = 'He.pr.1.6'
case obs of
;;--------------------------------------------
	'He.ar.1.1': begin
	_wave0 = '10830' & _exp0 = '15' & _rotp0 = '10' & _setname = 'ar.1.1'
	path.rootdir = 	_DATAROOT + 'spec/He_I_10830/' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp0+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp0+'r'+_rotp0+'_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e'+_exp0+'.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = path.workdir + path.outdir +	_wave0+'.pcal.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 100 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 90 		; position # in a scan for test
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
	
	my_i2quv = [0, 0., 0]   ; WEST with WEST pcal
	pmax0 = 0.02 
	end
;;--------------------------------------------
	'He.ar.1.2': begin
	_wave0 = '10830' & _exp0 = '15' & _rotp0 = '10' & _setname = 'ar.1.2'
	path.rootdir = 	_DATAROOT + 'spec/He_I_10830/' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp0+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp0+'r'+_rotp0+'_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e'+_exp0+'.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = path.workdir + path.outdir + '../ar.1.1/' +	_wave0+'.pcal.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 100 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 90 		; position # in a scan for test
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
	
	my_i2quv = [0, 0., 0]   ; WEST with WEST pcal
	pmax0 = 0.02 
	end
;;--------------------------------------------
	'He.pr.1.4': begin
	_wave0 = '10830' & _exp0 = '100' & _rotp0 = '20' & _setname = 'pr.1.4'
	path.rootdir = 	_DATAROOT + 'spec/He_I_10830/' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp0+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp0+'r'+_rotp0+'_*.fits'		; polarizer data in caldir
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

	dinfo.telpos="EAST"
	dinfo.nstep = 70 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 35 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.incli = 23.57	 	; inclination if dst_info not available
	;dinfo.th_offset = 33.53		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = ''			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0			; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = -1		; 0 - no correction, 1 - use i2quv[2,3],  2 - correct with own IQUV 

	my_i2quv = [-0.005, 0.028, 0.]  
	my_i2quv = [0.01, 0.00, 0.]     ; pcal 20220405
	pmax0 = 0.005
	end
;;--------------------------------------------
	'He.pr.1.6': begin
	_wave0 = '10830' & _exp0 = '60' & _rotp0 = '15' & _setname = 'pr.1.6'
	path.rootdir = 	_DATAROOT + 'spec/He_I_10830/' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp0+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp0+'r'+_rotp0+'_*.fits'		; polarizer data in caldir
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

	dinfo.telpos="EAST"
	dinfo.nstep = 5 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 3 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.incli = 302.45	 	; inclination if dst_info not available
	;dinfo.th_offset = 33.53		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = ''			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0			; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = -1		; 0 - no correction, 1 - use i2quv[2,3],  2 - correct with own IQUV 

	my_i2quv = [-0.005, 0.028, 0.]  
	my_i2quv = [-0.025, 0.00, -0.003]    ; pcal 20220405
	pmax0 = 0.005
	end
;;--------------------------------------------
    'cir': begin
	
	end
;;--------------------------------------------
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

