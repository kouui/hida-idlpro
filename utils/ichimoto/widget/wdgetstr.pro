;+
;  wdgetstr.pro (function)
;	wdgetstr
;	'97/10/01	k.i.
;	'06/12/23	k.i.,  no_modal keyword
;-
pro wdgetstr_event,ev
common wdgetstr,value
widget_control, ev.id, get_value=value, set_value=''
WIDGET_CONTROL, /destroy, ev.top
end

;--------------------------------------------------------
function wdgetstr,str,xpos=xpos,ypos=ypos,title=title,no_modal=no_modal
common wdgetstr,value

if not keyword_set(xpos) then xpos=0
if not keyword_set(ypos) then ypos=0
if not keyword_set(title) then title=''
if keyword_set(no_modal) then modal=0 else modal=1
base = WIDGET_BASE(title=title, /row) 

lab = widget_label(base,value=str);,font=2)
wdid = widget_text(base,value='', uvalue='Wdgetstr',/edit)
widget_control, base, /realize, tlb_set_xoffset=xpos,tlb_set_yoffset=ypos
XMANAGER, 'wdgetstr', base, modal=modal	; <= modal is impotant

return,value(0)
end
