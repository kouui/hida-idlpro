;+
;NAME:
;	RICE
;PURPOSE:
;	for reading Rice compressed or uncompressed FITS format
;	files
;SAMPLE CALLING SEQUENCES:
;	xfile=rice(name,ftype,hdr)
;INPUT:
;	filename of image to be read, ftype = 1 for simple FITS,
;	ftype = 2 for Rice compressed
;OUTPUT:
;	xfile will contain the image array
;	hdr will contain the file header; for FITS files, the 
;	header is returned as a string array of n strings of 80
;	characters.
;NOTES:
;	Rice and F0 decompression uses decrunch.c and associated
;	files (crunchstuff.c, anarw.c) and will write
;	decompressed results to a temporary file, then read from
;	this file.  To use this routine on outside systems, the
;	C code executables must be present and pathnames for
;	locating them may need to be changed in the IDL code.
;	The user may also wish to change the location of the
;	temporary file.  
;	The f0read and rice functions, as well as the decrunch
;	program, may be used individually.
;HISTORY:
;	Z. Frank 3/97, based on R. Shine's reading, writing, and
;	decompression routines
;       29-Apr-97 (MDM) - Modified how spawning to executable works
;                         (to use SSW_BIN routine)
;
;
;**************************************************************************
;
;-
function rice,name,ftype,hdr
;read rice compressed files or uncompressed fits files
noend=1 & nh=0 & dflag=0

;sflag tests if either data or machine is little endian
lendian=0
if (!version.os eq 'OSF') then lendian=1
sflag = lendian                         ;xor (subf lt 128)

;if compressed data, decrunch it
if ftype eq 2 then begin
    dflag=1

    ;--- get scratch file name
    scratch, lun, name=tmpfil           & if (keyword_set(lun)) then free_lun, lun

    ;--- get the executable location
    exe = ssw_bin('decrunch', found=found)
    if (not found) then message, 'Cannot find DECRUNCH.  Stopping...'

    ;---- build and execute command
    cmd = [exe, name, tmpfil]
    if (keyword_set(qdebug)) then print, cmd
    spawn, cmd, /noshell

    cmd = ['chmod', 'ugo+rw', tmpfil]
    spawn, cmd, /noshell
end

if ftype eq 1 then openr,unit,name,/GET_LUN else openr,unit,tmpfil,/GET_LUN

r=assoc(unit,bytarr(2880))
hdr=r(0)
dims=lonarr(8)

while noend eq 1 do begin
  for i=0,2880-1,80 do begin
    ss=hdr(i:i+79)
;look for end
    if string(hdr(i:i+3)) eq 'END ' then noend=0
;look for axes and fill in dimensions
    test=strpos(ss,'NAXIS')
    if test eq 0 then begin
      sq=strmid(ss,5,1)
      if sq ne ' ' then begin
        iq=fix(sq)
        if iq ge 0 or iq le 9 then $
          dims(iq-1)=fix(strmid(ss,strpos(ss,'=')+1,50))
      endif else naxis=fix(strmid(ss,strpos(ss,'=')+1,50))
                ;end of dimensions loop
    end         ;of test for naxis loop
;look for data type
    test=strpos(ss,'BITPIX')
    if test eq 0 then begin
       iq=fix(strmid(ss,strpos(ss,'=')+1,50))
       case iq of
       8 : datyp=0
       16 : datyp =1
       32 : datyp =2
       -32 : datyp =3
       -64 : datyp =4
       endcase
    end         ;of test for datatype loop
  end           ;of for i loop
  nh=nh+1
  hdr=r(nh)
end             ;of while loop

close,unit
FREE_LUN,unit
dims=dims(0:(naxis-1))
if ftype eq 1 then openr,unit,name,/GET_LUN else openr,unit,tmpfil,/GET_LUN

;load header for return
ns=nh*2880              ;number of header characters
nl=nh*36                ;number of header lines
r=assoc(unit,bytarr(ns))
hdr=r(0)
hdr=reform(hdr,80,nl)
hdr=string(hdr)

nq=1 & for i=0,naxis-1 do nq=nq*dims(i)

  case datyp of
  0:    begin
        nfh=ns
        r=assoc(unit,bytarr(nq+nfh))
        a=r(0)
        a=a(ns:*)
        end
  1:    begin
        nfh=ns/2
        r=assoc(unit,intarr(nq+nfh))
        a=r(0)
        a=a(nfh:*)
        if (sflag eq 1 and dflag eq 0) then byteorder,a
        end
  2:    begin
        nfh=ns/4
        r=assoc(unit,lonarr(nq+nfh))
        a=r(0)
        a=a(nfh:*)
        if (sflag eq 1 and dflag eq 0) then byteorder,/lswap,a
        end
  3:    begin
        nfh=ns/4
        r=assoc(unit,fltarr(nq+nfh))
        a=r(0)
        a=a(nfh:*)
        if sflag eq 1 then byteorder,/lswap,a
        end
  endcase

x=reform(a,dims)
close,unit
FREE_LUN,unit
if dflag then file_delete, tmpfil
return,x
end     ;rice function

;
;**************************************************************************
