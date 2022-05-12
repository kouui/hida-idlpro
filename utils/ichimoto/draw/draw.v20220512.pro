;+
; NAME       : draw.pro (procedure)
; PURPOSE :
;    draw lines with device coordinate
; CATEGORY :
;	idlpro/util
; CALLING SEQUENCE :
;	draw,x,y,line=line,frame=frame,thick=thick,color=color,psym=psym
; INPUTS :
;      x,y  : coordinate array
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
;	line	: kind of line
;	thick	: line thickness
;	color   : line color
;	frame   : [xmin,ymin,xmax,ymax]
;	psym    : plot symbol
; MODIFICATION HISTORY :
;	K.I. '95/02/23
;	K.I. '95/04/29	frame keyword
;	K.I. '95/05/07	color keyword
;	K.I. '98/03/13	psym keyword
;	K.I. '99/02/01	norm keyword
;-
pro draw,x,y,line=line,frame=frame,thick=thick,color=color,psym=psym, $
	norm=norm

if keyword_set(frame) then begin
	xx=(float(x)-frame(0))/(frame(2)-frame(0))*(!d.x_size-1)
	yy=(float(y)-frame(1))/(frame(3)-frame(1))*(!d.y_size-1)
endif else begin
	xx=x
	yy=y
endelse
if not keyword_set(line) then line=0
if not keyword_set(thick) then thick=1
if not keyword_set(psym) then psym=0
if n_elements(color) eq 0 then color=!p.color
if keyword_set(norm) then begin
    plot,xx,yy,/noerase,line=line,thick=thick,color=color,	$
	pos=[0,0,1,1],/norm,       $
        xstyle=1+4,xrange=[0.,1],     $
        ystyle=1+4,yrange=[0.,1],psym=psym
endif else begin
    plot,xx,yy,/noerase,line=line,thick=thick,color=color,	$
	pos=[0,0,!d.x_size-1,!d.y_size-1],/dev,       $
        xstyle=1+4,xrange=[0.,!d.x_size-1],     $
        ystyle=1+4,yrange=[0.,!d.y_size-1],psym=psym
endelse


end
