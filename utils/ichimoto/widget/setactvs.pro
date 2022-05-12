;+
;  setactvs.pro (function)
;	edit set of values and set ectivity of them
;-
pro setactvs_event,ev
common setactvs,bOK,wdb,wdm,mval,acval

i=where(ev.id eq wdb, count)
if count ne 0 then begin
	acval(i(0))=ev.select
endif
i=where(ev.id eq wdm, count)
if count ne 0 then begin
	mval(i(0))=gt_wdtxt(ev.id)
endif

if ev.id eq bOK then begin
	WIDGET_CONTROL, /destroy, ev.top
endif
end

pro setactvs,menu,actvs,title=title,xpos=xpos,ypos=ypos
common setactvs,bOK,wdb,wdm,mval,acval

if not keyword_set(xpos) then xpos=0
if not keyword_set(ypos) then ypos=0
if not keyword_set(title) then title=''
nn=n_elements(menu)
if n_elements(actvs) eq 0 then actvs=intarr(nn)
acval=actvs
mval=menu
wdm=lonarr(nn)
wdb=lonarr(nn)

base = WIDGET_BASE(title=title, /column, /frame ) 
for i=0,nn-1 do begin
	b1 = WIDGET_BASE(base, /row ) 
	bb = WIDGET_BASE(b1, /row, /nonexclusive, ysize=15, space=5) 
	wdb(i)=widget_button(bb,value='')
	wdm(i)=widget_text(b1,value=menu(i),/edit)
endfor
bOK = widget_button(base, value="OK", uvalue="OK")
widget_control, base, /realize, tlb_set_xoffset=xpos, tlb_set_yoffset=ypos
for i=0,nn-1 do widget_control,wdb(i),set_button=actvs(i)
;print,wdb

XMANAGER, 'setactvs', base, /modal	; <= modal is impotant

actvs=acval
menu=mval
ii=where(actvs eq 1, count)
if count ne 0 then print,menu(ii),' were set active...' $
else print,'nothing set active...'

end
