;+
;  mselect.pro (function)
;	create menu widget for selection
; CALLING SEQ.
;	result=mselect(menu,title=title,base=base0,wdid=wdid, $
;		pos=pos,/string,scr_ysize=scr_ysize)
; INPUT:
;	menu   --  menu string array
; KEYWORDS:
;	title  --  title
;	base   --  if set, menu is in base window
;	wdid   --  return widget ID
;	pos(2) --  position (or xpos, ypos)
;	string --  if set, return string value, else return No.
; MODIFICATION:
;	k.i., '04/08/15
;-
pro mselect_event,ev
common mselect,wd,isel

case ev.id of
  wd.LIST: begin
	isel=widget_info(ev.id, /list_select)
	end
  wd.All: begin
	nn=widget_info(wd.LIST, /list_number)
	isel=indgen(nn)
	widget_control, wd.LIST, set_list_select=isel
	end
  wd.Ok: begin
	WIDGET_CONTROL, /destroy, ev.top
	end
endcase

end

;--------------------------------------------------------
function mselect,menu,title=title,base=base0,wdid=wdid,	$
	pos=pos,string=string,xpos=xpos,ypos=ypos, $
	scr_ysize=scr_ysize,top=top
common mselect,wd,isel

if not keyword_set(title) then title=''
if not keyword_set(scr_ysize) then scr_ysize=700
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

wd={wd_mselect, $
	LIST: 0l, $
	ALL:  0l, $
	Ok:   0l $
	}

wd.LIST = widget_list(base,value=menu, kill_notify='', $
	uvalue="widget_list", ysize=n_elements(menu), scr_ysize=scr_ysize,/multi)
b1=widget_base(base,/row)
wd.ALL = widget_button(b1,value='All')
wd.Ok = widget_button(b1,value='Ok')


widget_control, base, /realize
XMANAGER, 'mselect', base, /modal	; <= modal is impotant

if keyword_set(string) then return,menu(isel)
return,isel

end
