;; ORCA th-offset experiment at 5890A
;; with camera link installed in bukkyoudai PC

@mmdst
@dst_pollib
@mmdst_lib
@quv_symfit
@undefine
@my_dst_pollib  ; overwrite functions in dst_pollib.pro

path = path_st()
dinfo = dinfo_st()
cal = calfile_st()

_DATAROOT = '/mnt/HDD3TBn52/DST/sp/20220503/'
_MY_WORKDIR = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'
_MY_FOLDER  = '20220503.orca-thoffset/'
_IMG_FMT = 'png'  ; 'png', 'gif'
_path_pcal_init = ''
_force_pcal_init = 0b
_check_polarizer = 0b
;---------------------------------------------------------------------------
; 

obs = 'Na.pol.bin1'
;obs = 'He.ar.2.3'
;obs = 'He.ar.5.1'
;obs = 'He.ar.2.2'
;obs = 'view'
case obs of
;;----[Na.pol.bin1]----------------------------------------
	'Na.pol.bin1': begin
	_wave0 = '5896' & _exp0 = '100' & _rotp0 = '40' & _setname = 'calib'
	path.rootdir = 	_DATAROOT + 'spec/Na_I_5890/bin1/' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = 'pol_e'+_exp0+'_r'+_rotp0+'_*.fits'				; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e15r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp0+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e50_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp0+'_r'+_rotp0+'_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = 'bin1.pol/'				; directory for obs.data output in workdir
	path.calpardir = 'bin1.cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e'+_exp0+'.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = path.workdir + path.outdir +	_wave0+'.pcal.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = -1 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 0 		; position # in a scan for test
	dinfo.wl0 = float(_wave0) 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 0.0		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	_check_polarizer = 0b
	_force_pcal_init = 0b
	_path_pcal_init = '';path.workdir + path.outdir + '/../ar.2.2/'  +	_wave0+'.pcal.sav'
	my_i2quv = [0, 0., 0]   ; WEST with WEST pcal
	pmax0 = 0.02 
	end
;;----[Na.pol.bin2]----------------------------------------
	'Na.pol.bin2': begin
	_wave0 = '5896' & _exp0 = '30' & _rotp0 = '40' & _setname = 'calib'
	path.rootdir = 	_DATAROOT + 'spec/Na_I_5890/bin2/' ; root directory of raw dataset
	path.obsdir = _setname+'/'				; holder of obs dataset
	path.obsdat = 'pol_e'+_exp0+'_r'+_rotp0+'_*.fits'				; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e15r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e'+_exp0+'_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e'+_exp0+'_r'+_rotp0+'_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = 'bin2.pol/'				; directory for obs.data output in workdir
	path.calpardir = 'bin2.cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e'+_exp0+'.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = path.workdir + path.outdir +	_wave0+'.pcal.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = -1 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 1 		; position # in a scan for test
	dinfo.wl0 = float(_wave0) 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 0.0		; inclination if dst_info not available
	;dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = 2		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	_check_polarizer = 0b
	_force_pcal_init = 0b
	_path_pcal_init = '';path.workdir + path.outdir + '/../ar.2.2/'  +	_wave0+'.pcal.sav'
	my_i2quv = [0, 0., 0]   ; WEST with WEST pcal
	pmax0 = 0.02 
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

