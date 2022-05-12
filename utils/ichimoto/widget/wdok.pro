;+
;  wdok.pro (function)
;	display ok widget
;	'99/12/04	k.i.
;	'04/04/12	k.i.  edit keyword
;	'04/04/16	k.i.  nowait keyword
;-
pro wdok_event,ev
common wdok,wdokid
if ev.id eq wdokid then $
WIDGET_CONTROL, /destroy, ev.top
end

;--------------------------------------------------------
pro wdok,str,xpos=xpos,ypos=ypos,title=title,edit=ed,nowait=nowait
common wdok,wdokid

ns=n_elements(str)
w=max(strlen(str))
if keyword_set(nowait) then modal=0 else modal=1
if not keyword_set(ed) then ed=0
if not keyword_set(xpos) then xpos=400
if not keyword_set(ypos) then ypos=300
if not keyword_set(title) then title=''
base = WIDGET_BASE(title=title, /column) 

;lab=widget_text(base,value=str,font=2,xsize=strlen(str[0]),ysize=n_elements(str),edit=ed)
lab=widget_text(base,value=str,font='-adobe-courier-medium-r-normal--25-180-100-100-m-150-iso8859-1', $
	xsize=strlen(str[0]),ysize=n_elements(str),edit=ed)
;if keyword_set(ed) then begin
;	for i=0,ns-1 do lab = widget_text(base,value=str(i),font=2,xsize=strlen(str(i)))
;endif else begin
;	for i=0,ns-1 do lab = widget_label(base,value=str(i),font=2)
;endelse

wdokid = widget_button(base, value="OK", uvalue = "OK")

widget_control, base, /realize, tlb_set_xoffset=xpos,tlb_set_yoffset=ypos
XMANAGER, 'wdok', base, modal=modal	; <= modal is impotant


end
