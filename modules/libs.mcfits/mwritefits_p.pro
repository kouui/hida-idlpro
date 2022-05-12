
;**************************************************************
pro mwritefits_p,imgs,p,file=file,s4pos=s4pos,rotp=rotp,time_arr=time_arr, $
	dst_status=dst_status, trig_arr=trig_arr, frate_arr=frate_arr, $
        filter=filter, binxy=binxy

;  2021.07.10   k.i., y.h.  from orcalib.pro
;  2021.08.17   k.i., k.u.  added time_arr keyword
;  2021.08.28   k.u., k.i.  added dst_status keyword 
;  2021.10.01   k.i., k.u.  added keyword `s4pos` and `rotp`
;  2021.12.23   k.u.        added keyword `filter` for filter header
;  2021.12.27   k.u         added keyword `binxy` for vimba interface


  ;p={orca4_param,   $
  ;   expo:        0.1,     $   ; exposure time [sec]
  ;   framerate:   float(30),       $   ; frame rate [frame/sec]
  ;   gain:        0,       $   ; gain 0-28
  ;   bin:          1,        $   ; Binning XY 1-8
  ;   Width:       2048,          $   ; Width  max=2048 (binx=1)
  ;   Height:      2048,          $   ; Height  max=2048 (biny=1)
  ;   RegionX:     0,             $   ; start of region read out,pixel,left edge
  ;   RegionY:     0,             $   ; start of region read out,pixel,top edge
  ;   Sparse:      0l,             $   ; if set, sample alternative images
  ;   TrigMode:    'Internal',    $   ; trigger mode, 'Internal' or 'Start'
  ;   TrigPol:  'Negative', $   ; trigger polarity, 'Negative' or 'Positive'
  ;   clock:       79861111l,   $   ; TimeStanmpFrequency [Hz]
  ;   timelo:      0,             $   ; Time stamp, lower 32-bits
  ;   timehi:      0,             $   ; Time stamp, upper 32-bits
  ;   status:      0            $   ; Status

  case size(imgs,/type) of
    1: bitpix=8
    2: bitpix=16
    3: bitpix=32
    4: bitpix=32
    5: bitpix=64
    6: bitpix=64  ; ? complex
    7: bitpix=-1  ; string
    8: bitpix=-1  ; struct
    9: bitpix=128 ; ? dcomplex
    10: bitpix=-1  ; pointer
    11: bitpix=-1  ; objref
    12: bitpix=16  ; uint
    13: bitpix=32  ; ulong
    14: bitpix=64  ; long64
    15: bitpix=64  ; ulong64
  endcase
  nax=size(imgs,/n_dimension)
  dim=size(imgs,/dimension)
  nax1=dim[0]
  if nax ge 2 then nax2=dim[1] else nax2=1
  if nax ge 3 then nax3=dim[2] else nax3=1

  fh=strarr(36)
  fh[0] =string('SIMPLE  = ','T',format='(a10,a20," /")')
  fh[1] =string('BITPIX  = ',bitpix,format='(a10,i20," /")')
  fh[2] =string('NAXIS   = ',nax,format='(a10,i20," /")')
  fh[3] =string('NAXIS1  = ',nax1,format='(a10,i20," /")')
  fh[4] =string('NAXIS2  = ',nax2,format='(a10,i20," /")')
  fh[5] =string('NAXIS3  = ',nax3,format='(a10,i20," /")')
  fh[6] =string('DATE_OBS= ',p.date_obs,format='(a10,a23," /")')
  fh[7] =string('DATE_OB2= ',p.date_obs2,format='(a10,a23," /")') ; end time of integ
  fh[8] =string('TIMESYS = ',p.timesys,format='(a10,a20," /")')
  fh[9] =string('OBSERVAT= ',p.observat,format='(a10,a20," /")')
  fh[10]=string('TELESCOP= ',p.telescop,format='(a10,a20," /")')
  fh[11]=string('CAMERA  = ',p.camera,format='(a10,a20," /")')
  fh[12]=string('EXP     = ',p.expo,format='(a10,f20.10," /")')
  fh[13]=string('DATA_TYP= ',p.data_typ,format='(a10,a20," /")')
  fh[14]='VARTYPE = UINT'
  fh[15]=string('X0      = ',p.RegionX,format='(a10,i20," /")')
  fh[16]=string('Y0      = ',p.RegionY,format='(a10,i20," /")')
  fh[17]=string('BIN     = ',p.bin,format='(a10,i20," /")')
  fh[18]=string('TRIGMODE= ',p.TrigMode,format='(a10,a20," /")')
  fh[19]=string('TRIGPOL = ',p.TrigPol,format='(a10,a20," /")')
  fh[20]=string('SPARSE  = ',p.sparse,format='(a10,a20," /")')

  index_last = 20

  if keyword_set(binxy) then begin
    fh[index_last+1]=string('BINX    = ',p.binx,format='(a10,i20," /")')
    fh[index_last+2]=string('BINY    = ',p.biny,format='(a10,i20," /")')
    index_last = index_last + 2
  endif
  
  if keyword_set(filter) then begin
     fh[index_last+1]='FILTER  = '+filter.filt_name
     fh[index_last+2]=string('WAVE0   = ',filter.wl0,format='(a10,f20.2," / nm")')
     fh[index_last+3]=string('DWL     = ',filter.dwl,format='(a10,f20.4," / nm")')
     index_last = index_last + 3
  endif
  
  fh[index_last+1]='COMMENT = none'
  fh[index_last+2]='HISTORY = RAW'
  index_last = index_last + 2

  if n_elements(s4pos) ne 0 then begin    ; header information for S4 position , integer
    index_last = index_last + 1
    fh[index_last]=string('S4POS   = ',s4pos,format='(a10,i20," /")')
  endif 

  if keyword_set(rotp) then begin        ; header information for rotation period of rotating waveplate, float
    index_last = index_last + 1  
    fh[index_last]=string('ROTP    = ',rotp,format='(a10,f20.4," /")')
  endif


  blnk80=string(' ',format='(a80)')
  fh=strmid(fh+blnk80,0,80)

  mwrfits,imgs,file,fh[0:index_last]

  if keyword_set(dst_status) then begin  ;-- uintarr of dst status
        fh[0] =string('EXTNAME = ',"'DST_STATUS'",format='(a10,a20," /")')
	fh[1] =string('BITPIX  = ',16,format='(a10,i20," /")')
	fh[2] =string('NAXIS   = ',2,format='(a10,i20," /")')	; 
	fh[3] =string('NAXIS1  = ',3,format='(a10,i20," /")')	; degree, minute, second
	fh[4] =string('NAXIS2  = ',10,format='(a10,i20," /")'); 10 status parameter
	fh[5] =string('PCOUNT  = ',0,format='(a10,i20," /")')
	fh[6] =string('GCOUNT  = ',1,format='(a10,i20," /")')
	fh[7:35]=string(' ',format='(a10)')
        ;fh[35]=string('END       ',format='(a10)')
	fh=strmid(fh+blnk80,0,80)
	mwrfits,status2array(dst_status),file,fh[0:6]
  endif	

  if keyword_set(time_arr) then begin	;-- time info from FLIR cam. 
	fh[0] =string('EXTNAME = ',"'TIME_ARR'",format='(a10,a20," /")')
	fh[1] =string('BITPIX  = ',16,format='(a10,i20," /")')
	fh[2] =string('NAXIS   = ',2,format='(a10,i20," /")')	; 
	fh[3] =string('NAXIS1  = ',4,format='(a10,i20," /")')	; hh mm ss ms
	fh[4] =string('NAXIS2  = ',nax3,format='(a10,i20," /")'); nimg
	fh[5] =string('PCOUNT  = ',0,format='(a10,i20," /")')
	fh[6] =string('GCOUNT  = ',1,format='(a10,i20," /")')
	fh[7:35]=string(' ',format='(a10)')
        ;fh[35]=string('END       ',format='(a10)')
	fh=strmid(fh+blnk80,0,80)
	mwrfits,time_arr,file,fh[0:6]
  endif

  if keyword_set(trig_arr) then begin	
	fh[0] =string('EXTNAME = ',"'TRIG_ARR'",format='(a10,a20," /")')
	fh[1] =string('BITPIX  = ',16,format='(a10,i20," /")')
	fh[2] =string('NAXIS   = ',1,format='(a10,i20," /")')	; 
	fh[3] =string('NAXIS1  = ',nax3,format='(a10,i20," /")'); nimg
	fh[4] =string('PCOUNT  = ',0,format='(a10,i20," /")')
	fh[5] =string('GCOUNT  = ',1,format='(a10,i20," /")')
	fh[6:35]=string(' ',format='(a10)')
        ;fh[35]=string('END       ',format='(a10)')
	fh=strmid(fh+blnk80,0,80)
	mwrfits,trig_arr,file,fh[0:5]
  endif

  if keyword_set(frate_arr) then begin
	fh[0] =string('EXTNAME = ',"'FR_ARR'",format='(a10,a20," /")')
	fh[1] =string('BITPIX  = ',32,format='(a10,i20," /")')
	fh[2] =string('NAXIS   = ',1,format='(a10,i20," /")')	; 
	fh[3] =string('NAXIS1  = ',nax3,format='(a10,i20," /")'); nimg
	fh[4] =string('PCOUNT  = ',0,format='(a10,i20," /")')
	fh[5] =string('GCOUNT  = ',1,format='(a10,i20," /")')
	fh[6:35]=string(' ',format='(a10)')
        ;fh[35]=string('END       ',format='(a10)')
	fh=strmid(fh+blnk80,0,80)
	mwrfits,frate_arr,file,fh[0:5]
  endif


end
