;+
;  wd_buttons.pro (function)
;	'03/03/31	k.i.
;	'07/07/22	k.i.	a bug
;-
pro wd_buttons_event,ev
common wd_buttons,wd1,wd2,wdex,vals

case ev.id of
   wd1: begin
	vals(0)=ev.value
	end
   wd2: begin
	vals(1)=ev.value
	end
   wdex: begin
	WIDGET_CONTROL, /destroy, ev.top
	return
	end
endcase

end

;--------------------------------------------------------
function wd_buttons,label1,sel1,label2,sel2,initval=initvals,xpos=xpos,ypos=ypos,title=title
common wd_buttons,wd1,wd2,wdex,vals

;label1='input'
;sel1=['Fx','Fy','Fz','Tx','Ty','Tz']
;label2='direction'
;sel2=['tx','ty','dx','dy','dz']

if not keyword_set(xpos) then xpos=0
if not keyword_set(ypos) then ypos=0
if not keyword_set(title) then title=''
base = WIDGET_BASE(title=title, /column) 

if keyword_set(initvals) then begin
	vals=initvals
endif else begin
	if keyword_set(sel2) then vals=[0,0] else vals=0
endelse
wd1 = cw_bgroup(base,sel1,/exclusive,/no_release,label_top=label1, $
		/row,/frame,set_value=vals(0))
if keyword_set(sel2) then begin
	wd2 = cw_bgroup(base,sel2,/exclusive,/no_release,label_top=label2, $
		/row,/frame,set_value=vals(1))
endif else wd2=-1
wdex = widget_button(base, value="OK", uvalue = "OK")

widget_control, base, /realize, tlb_set_xoffset=xpos,tlb_set_yoffset=ypos
XMANAGER, 'wd_buttons', base, /modal	; <= modal is impotant

return,vals

end
