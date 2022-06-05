@import.dstpol

path = path_st()
dinfo = dinfo_st()
cal = calfile_st()

_DATAROOT = '/mnt/HDD3TBn57/DST/sp/20220529/'
_MY_WORKDIR = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'
_MY_FOLDER  = '20220529.filament-utf/'
_IMG_FMT = 'png'  ; 'png', 'gif'
_path_pcal_init = ''
_force_pcal_init = 0b
_check_polarizer = 0b
_NCORE = 4
undefine, _ysiz
;---------------------------------------------------------------------------
; 

obs = 'He.df.1.7'

case obs of
;;----[He.df.1.6]----------------------------------------
	'He.df.1.6': begin
	_wave0 = '10830' & _exp0 = '15' & _rotp0 = '10' & _setname = 'df.1.6'
	path.rootdir = 	_DATAROOT + '' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e15r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'drk15_*.fits'		; dark file path in caldir
	path.flatfile = 'flt_*.fits'			; flat file path in caldir
	path.flatdark = 'drk15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_*.fits'		; polarizer data in caldir
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
	dinfo.i_scan = 0 			; scan # for testing cal.
	dinfo.j_pos = 50;2 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 282.983		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0			; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	_check_polarizer = 0b
	_force_pcal_init = 0b
	_path_pcal_init = '';path.workdir + path.outdir + '/../ar.2.2/'  +	_wave0+'.pcal.sav'
	my_i2quv = [0, 0., 0]   ; WEST with WEST pcal
	pmax0 = 0.002 
	end
;;----[He.df.1.7]----------------------------------------
	'He.df.1.7': begin
	_wave0 = '10830' & _exp0 = '15' & _rotp0 = '10' & _setname = 'df.1.7'
	path.rootdir = 	_DATAROOT + '' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e15r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'drk15_*.fits'		; dark file path in caldir
	path.flatfile = 'flt_*.fits'			; flat file path in caldir
	path.flatdark = 'drk15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_*.fits'		; polarizer data in caldir
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

	dinfo.nstep = 1000 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 			; scan # for testing cal.
	dinfo.j_pos = 50;2 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 282.983		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0			; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	_check_polarizer = 0b
	_force_pcal_init = 1b
	_path_pcal_init = path.workdir + path.outdir + '/../df.1.6/'  +	_wave0+'.pcal.sav'
	my_i2quv = [0, 0., 0]   ; WEST with WEST pcal
	pmax0 = 0.002
	_ysiz = 600 
	end

;;----[cir]----------------------------------------
    'cir': begin
	
	end
;;--------------------------------------------
endcase

@main.dstpol

stop





end

