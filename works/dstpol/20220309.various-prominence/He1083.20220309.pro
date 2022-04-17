; He1083.20220309.pro
@mmdst
@dst_pollib
@mmdst_lib
@quv_symfit
@undefine

function srot, s, phi
;; phi in [deg]
	phi2 = 2 * phi /180*!pi
	c2 = cos(phi2) & s2 = sin(phi2)
	s0 = s
	s0[*,*,1] = s[*,*,1] *  c2   + s[*,*,2] * s2
	s0[*,*,2] = s[*,*,1] * (-s2)   + s[*,*,2] * c2
	return, s0
end

path = path_st()
dinfo = dinfo_st()
cal = calfile_st()

_MY_WORKDIR = '/tmp_mnt/nwork/kouui/data-lvl1/dstpol/'
_MY_FOLDER  = '20220309.various-prominence/'
_IMG_FMT = 'png'  ; 'png', 'gif'
;---------------------------------------------------------------------------
obs = 'He.ar.1.4'
case obs of
   'He.ar.1.4': begin
	_wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn51/DST/sp/20220309/spec/He_I_10830/'	; root directory of raw dataset
	path.obsdir = 'ar.1.4/'				; holder of obs dataset
	path.obsdat = '*.fits'				; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e15r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e15_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_r1e15_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER	; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e15.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap   = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo= path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = calpardir+	_wave0+'.pcal.EAST.sav'	; polari. calib params.
	cal.wl   = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.nstep = 120 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 100 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 5.733		; inclination if dst_info not available
	dinfo.th_offset = 26.5		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = 'c'			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0		; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = -1		; 0 - no correction, 1 - use pcal.i2quv[2,3],  2 - get i2quv from itself
	
	my_i2quv = [0.001, -0.0025, -0.0005] 
	pmax0 = 0.01 
	end
   'He.pr.1.4': begin
   	_wave0 = '10830'
	path.rootdir = '/mnt/HDD3TBn53/DST/sp/20220408/spec/He_I_10830/'	; root directory of raw dataset
	path.obsdir = 'pr.1.4/'				; holder of obs dataset
	path.obsdat = '*.fits'		; observation seq. data
	;path.obsdir = 'calib/'				; holder of obs dataset
	;path.obsdat = 'cir_e50_r10*.fits'		; observation seq. data
	path.caldatdir = 'calib/'			; holder of clib. data
	path.darkfile = 'dark_e100_*.fits'		; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = 'dark_e15_*.fits'		; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol_e100_r20_*.fits'		; polarizer data in caldir
	path.workdir = _MY_WORKDIR + _MY_FOLDER			; output root directory in /nwork
	path.outdir = path.obsdir				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir

	calpardir = path.workdir+path.calpardir
	cal.dark = calpardir+	_wave0+'.dark.e100.sav'		; drk[nx,ny]
	cal.flat = calpardir+	_wave0+'.flat.sav'		; fltl,fltr,avfltl,avfltr,fltspl,fltspr [nxp,ny] 
	cal.ap = calpardir+	_wave0+'.ap.sav'		; alignment params.
	cal.dinfo = path.workdir + path.outdir +	'dinfo.sav'	; data info.
	cal.pcal = calpardir+	_wave0+'.pcal.EAST.20220405.sav'	; polari. calib params.
	cal.wl = calpardir+	_wave0+'.wl.sav'		; wavelength wl[*]

	if file_test(cal.dinfo) then restore,cal.dinfo

	dinfo.telpos="EAST"
	;dinfo.telpos="WEST"
	dinfo.nstep = 50 		; # of steps for 1 scan, if -1 do all data in obsdir
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 25 		; position # in a scan for test
	dinfo.wl0 = 10830. 		; central wavelength [A]
	dinfo.incli = 6.10	 	; inclination if dst_info not available
	dinfo.th_offset = 32.75		; from #1, offset angle of waveplate to eliminate R, deg.
	dinfo.div = ''			; divide QUV by continuum 'c',  intensity 'i', and non ''
	dinfo.xalign = 0			; align in slit direction
	dinfo.adj_dstpol = 0		; if 1, adjust DST pol.parms using Zeemen in sunspot 
	dinfo.correct_I2quv = -1		; 0 - no correction, 1 - use i2quv[2,3],  2 - correct with own IQUV 

	my_i2quv = [0.02, 0.0, 0.]   
	pmax0 = 0.001
	end

   


endcase


@dstpolcal_body








end

