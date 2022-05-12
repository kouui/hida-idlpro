pro imscrl_event, ev
common imscrl,Idrw,Iexit

case (ev.id) of
   Iexit: begin
	WIDGET_CONTROL, /destroy, ev.top
	return
	end
endcase
end

;------------------------------------------------------
pro imscrl,img
common imscrl,Idrw,Iexit

base = WIDGET_BASE(title='ImScrl test', /column)
b1 = widget_base(base, /row, /frame )	
Idrw = widget_draw(b1, xsize=1024, ysize=1024, $	; <=img size
	 x_scroll_size=512, y_scroll_size=512, /scroll) ; <=window size
Iexit = widget_button(base, value="Exit",uvalue="Exit")
widget_control, base, /realize

widget_control, Idrw,get_value=wid &	wset,wid
tvscl,img

XMANAGER, 'imscrl', base


end
