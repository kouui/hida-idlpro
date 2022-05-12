;+
;  wd_pickfiles.pro (function)
;	select files or directories
;	# 'dialog_pickfile' cannot select multipule directories
; CALLING SEQ.
;	files=wd_pickfiles(initpath=initpath)
; INPUT:
; KEYWORDS:
;	initpath  --  initial path
; MODIFICATION:
;	k.i., 2006/12/14
;	k.i., 2006/12/16  bug fix
;	k.i., 2006/12/22  bug fix
;-
;********************************************************************
pro wd_pickf_event,ev
common wd_pickf,wd,dir,files,isel,isel0

case !version.os_family of
  'Windows': begin 
	div='\'
	end
  'unix': begin 
	div='/'
	end
endcase

chdir=0
case (ev.id) of
   wd.PATH:   begin
	widget_control, ev.id, get_value=value, set_value=''
	widget_control, ev.id, set_value=value(0)
	dir=value(0)
	chdir=1
	end
   wd.LIST: begin
	isel=widget_info(ev.id, /list_select)
	nsel=n_elements(isel)
	nsel0=n_elements(isel0)
	if nsel eq 1 and nsel0 eq 1 then begin
		if isel eq isel0 then begin	; duble click
			file1=files(isel)
			char9=strmid(file1,strlen(file1)-1,1)
			if char9 eq div then begin ; new directory
				chdir=1
				if file1 eq '..'+div then begin
					dir0=strmid(dir,0,strlen(dir)-1)
					filename_sep,dir0,dir
					dir=dir+div
				endif else begin
					dir=dir+file1
				endelse
			endif
		endif
	endif
	isel0=isel
	end
   wd.EXIT:   begin
	WIDGET_CONTROL, /destroy, ev.top
	return
	end
   else:
endcase

if chdir then begin
	widget_control, wd.PATH, set_value=dir
	if float(!version.release) lt 6.1 then begin
		fulfiles=findfile(dir+'*')
	endif else begin
		fulfiles=file_search(dir+'*',/fully_qualify_path)
		finfo=file_info(fulfiles)
		ii=where(finfo.directory eq 1, ndir)
		if ndir ne 0 then fulfiles(ii)=fulfiles(ii)+div
	endelse
	nn=n_elements(fulfiles)
	files=strarr(nn)
	ldir=strlen(dir)
	for i=0,nn-1 do files(i)=strmid(fulfiles(i),ldir,strlen(fulfiles(i))-ldir)
	if float(!version.release) ge 6.1 then files=['..'+div,files]
	widget_control, wd.LIST, set_value=files
endif

end

;****************************************************************************
function wd_pickfiles,initpath=initpath,dir=dir1
common wd_pickf,wd,dir,files,isel,isel0

case !version.os_family of
  'Windows': begin 
	div='\'
 	dir0='c:\data\hinode\'
	end
  'unix': begin 
	div='/'
	dir0='/isas/data/solarb_fits/'
	end
endcase
if keyword_set(initpath) then dir=initpath else dir=dir0

wd={wd_pickf, $
	PATH:	0l, $
	LIST: 	0l, $
	EXIT:	0l $
	}

if float(!version.release) lt 6.1 then begin
	fulfiles=findfile(dir+'*')
endif else begin
	fulfiles=file_search(dir+'*',/fully_qualify_path)
	finfo=file_info(fulfiles)
	ii=where(finfo.directory eq 1, ndir)
	if ndir ne 0 then fulfiles(ii)=fulfiles(ii)+div
endelse
nn=n_elements(fulfiles)
files=strarr(nn)
ldir=strlen(dir)
for i=0,nn-1 do files(i)=strmid(fulfiles(i),ldir,strlen(fulfiles(i))-ldir)
if float(!version.release) ge 6.1 then files=['..'+div,files]

base = WIDGET_BASE(title='wd_pickfiles',  /column) 
b1=WIDGET_BASE(base,/row,/frame) 
lab = widget_label(b1,value='Path: ')
wd.PATH = widget_text(b1,value=dir, xsize=50, uvalue='PATH',/edit)

wd.LIST = widget_list(base,value=files, kill_notify='', $
	uvalue="widget_list", ysize=nn, scr_ysize=600,/multi)
wd.EXIT = widget_button(base,value='Exit')

widget_control, base, /realize
XMANAGER, 'wd_pickf', base ;, /modal	; <= modal is impotant

dir1=dir
ff=dir+files(isel)
return,ff


end
