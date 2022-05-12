;+
;  smenu.pro (function)
;	wmenu
;	create mennu widget
; CALLING SEQ.
;	result=smenu(menu,title=title,pd=pd,base=base0,wdid=wdid, $
;		pos=pos,/string,scr_ysize=scr_ysize,multi=multi)
; INPUT:
;	menu   --  menu string array
; KEYWORDS:
;	title  --  title
;	base   --  if set, menu is in base window
;	wdid   --  return widget ID
;	pos(2) --  position (or xpos, ypos)
;	string --  if set, return string value, else return No.
;	pd  --     if set, pulldown menu
;	button --  if set, button menu
; MODIFICATION:
;	k.i., '96/10/28
;	k.i., '99/05/02
;	k.i., '01/12/21
;	k.i., '04/08/15   not work
;	k.i., '07/04/21   list multi
;	k.i.,k.u., 2022/05/07   list font
;   2022.05.12  k.u.  removed /model keyword in xmanager; changed font
;-
pro smenu_event,ev
common smenu,bMenu,value,list

;widget_control, ev.id, get_uvalue=values
if keyword_set(list) then begin
	value=widget_info(ev.id, /list_select)
	;value=ev.index
endif else begin
	value=ev.value
endelse
WIDGET_CONTROL, /destroy, ev.top

end

;--------------------------------------------------------
function smenu,menu,title=title,base=base0,wdid=wdid,	$
	pos=pos,string=string,pd=pd,button=button,xpos=xpos,ypos=ypos, $
	scr_ysize=scr_ysize,multi=multi
common smenu,bMenu,value,list

if not keyword_set(title) then title=''
if not keyword_set(scr_ysize) then scr_ysize=0
if not keyword_set(multi) then multi=0
if not keyword_set(pos) then begin
	xoffset=0
	yoffset=0
endif else begin
	xoffset=pos(0)
	yoffset=pos(1)
endelse
if keyword_set(xpos) then xoffset=xpos
if keyword_set(ypos) then yoffset=ypos
if not keyword_set(base0) then $
	base = WIDGET_BASE(title=title, 	$
		xoffset=xoffset, yoffset=yoffset, /column) $
else 	base = WIDGET_BASE(base0,title=title, 	$
		xoffset=xoffset, yoffset=yoffset, /column) 

list=0
if keyword_set(pd) then begin
	desc='0\'+menu
	wdid = cw_pdmenu(base,desc,return_name=0,uvalue='Cw_pdmenu')
endif else if keyword_set(button) then begin
	wdid = cw_bgroup(base,menu,/column,label_top='', $
		uvalue="Cw_bgroup",nonexclusive=multi)
endif else begin
	font = 'terminus-iso8859-9-32'
	list=1
	wdid = widget_list(base,value=menu, kill_notify='', $
		uvalue="widget_list", ysize=n_elements(menu), scr_ysize=scr_ysize, $
		multi=multi,xsize=max(strlen(menu))+3,font=font)
endelse

widget_control, base, /realize
XMANAGER, 'smenu', base;, /modal	; <= modal is impotant
;XMANAGER, 'smenu', base, no_block=0	; <= modal is impotant, removed 2006.12.23

;print,value
if keyword_set(string) then return,menu(value)
return,value
end


