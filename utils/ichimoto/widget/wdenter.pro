;+
;  wdenter.pro (function)
;	Enter a param from widget
;	'05/09/10	k.i.  
;-
pro wdenter_event,ev
common wdenter,wdenterid,value
if ev.id eq wdenterid then $
	widget_control, ev.id, get_value=value, set_value=''
WIDGET_CONTROL, /destroy, ev.top
end

;--------------------------------------------------------
function wdenter,str,xpos=xpos,ypos=ypos,title=title,edit=ed,nowait=nowait
common wdenter,wdenterid,value

ns=n_elements(str)
w=max(strlen(str))
if keyword_set(nowait) then modal=0 else modal=1
if not keyword_set(ed) then ed=0
if not keyword_set(xpos) then xpos=400
if not keyword_set(ypos) then ypos=300
if not keyword_set(title) then title='wdenter'
base = WIDGET_BASE(title=title, /column) 

lab=widget_text(base,value=str,font=2,xsize=w,ysize=n_elements(str))

b1 = WIDGET_BASE(base, /row) 
lab=widget_text(b1,value='Enter=> ',font=2,xsize=8)
wdenterid = widget_text(b1,value='',uvalue='Wdenter',xsize=10, /edit)

widget_control, base, /realize, tlb_set_xoffset=xpos,tlb_set_yoffset=ypos
XMANAGER, 'wdenter', base, modal=modal	; <= modal is impotant

return,value

end
