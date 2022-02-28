; dstpol_20220209_NaD_limb.pro
@mmdst
@dst_pollib
@mmdst_lib
@quv_symfit
@undefine

path = path_st()
dinfo = dinfo_st()

;---------------------------------------------------------------------------

obs = 'NaD'
case obs of
   'NaD': begin
	path.rootdir = '/mnt/HDD3TBn51/DST/sp/20220209/spec/'	; root directory of raw dataset
	path.obsdir = 'NaI5896/ar2/'			; holder of obs dataset
	path.caldatdir = 'NaI5896/calib/'		; holder of clib. data
	path.darkfile = '../../dark/dark_orca_50ms_*.fits'	; dark file path in caldir
	path.flatfile = 'flat_*.fits'			; flat file path in caldir
	path.flatdark = ''				; dark for flat in caldir if expo. is different 
	path.hairline = 'hair*.fits'			; hairline data in caldir 
	path.polarizer = 'pol*.fits'			; polarizer data in caldir
	path.obsdat = '*.fits'				; observation seq. data
	path.workdir = '/nwork/ichimoto/20220209_pol/'	; output root directory in /nwork
	path.outdir = 'NaD/'				; directory for obs.data output in workdir
	path.calpardir = 'cal/'				; directory for saving calib.params in workdir
	path.calid = 'Na'				; ID of calibration files .sav

	calpardir = path.workdir+path.calpardir
	dinfo_sav = calpardir+'dinfo_'+path.calid+'.sav'
	if file_test(dinfo_sav) then restore,dinfo_sav

	dinfo.nstep = 9 		; # of steps for 1 scan
	dinfo.i_scan = 0 		; scan # for testing cal.
	dinfo.j_pos = 3 		; position # in a scan for test
	dinfo.wl0 = 5893. 		; central wavelength [A]
	dinfo.wl_range = 15.		; apporox. wavelength range [A] 	
	dinfo.wl_order = 1.		; 0: top is red, 1: top is blue  -> left sp is +U or -U
	dinfo.telpos = 'WEST'		; DST position,  'WEST' or 'EAST',  get from dst_info
	dinfo.incli = 92.82		; inclination if dst_info not available
	dinfo.th_offset = 37.		; offset angle of waveplate to eliminate R, deg.

	path.obsdir = 'NaI5896/limb1/'	; holder of obs dataset
	path.outdir = 'NaD_limb/'	; directory for obs.data output in workdir
	dinfo.nstep = 30		; # of steps for 1 scan

	end
endcase


pmax = 0.01	 ; max p for display

@dstpolcal_body

end

