;+
; NAME:
;	wdplopt (procedure)
; FUNCTION:
;	plot array with a controling widget
; USAGE:
;	wdplot,x,y
; KEYWORDS
; HISTORY:
;	99/12/01  k.i.	
;-
@wd_sldbx
;*******************************************************************
pro plotp,x,y,p
; plot y vs x  according to p-struct
	xdd=!x.range(0)+[1,1+(!x.range(1)-!x.range(0))*0.01] &	ydd=!y.range(0)+[0,0]
	plot,x,y(*,0),xtitle=p.x.title,ytitle=p.y.title,xstyle=1,ystyle=1
	oplot,xdd,ydd,line=0
	imgsize,y,n0,n1,n2
	line=0
	for i=1,n1-1 do begin
		if p.incline eq 1 then line=i
		oplot,x,y(*,i),line=line
		oplot,xdd,ydd,line=line
	endfor
	for j=1,n2-1 do begin
		if p.incline eq 2 then line=j
		for i=0,n1-1 do begin
			if p.incline eq 1 then line=i
			oplot,x,y(*,i,j),line=line
			oplot,xdd,ydd,line=line
		endfor
	endfor
end

;*******************************************************************
pro wdplot_event,ev
common wdplot,wdp,p,x,y

plt=0

if ht_sldbx(ev, wdp.xmax, p.xval, setv, format=p.xform) then begin
	!x.range(1)=setv
	plt=1
endif
if ht_sldbx(ev, wdp.xmin, p.xval, setv, format=p.xform) then begin
	!x.range(0)=setv
	plt=1
endif
if ht_sldbx(ev, wdp.ymax, p.yval, setv, format=p.yform) then begin
	!y.range(1)=setv
	plt=1
endif
if ht_sldbx(ev, wdp.ymin, p.yval, setv, format=p.yform) then begin
	!y.range(0)=setv
	plt=1
endif

case ev.id of
   wdp.Quit:  begin
	WIDGET_CONTROL, /destroy, ev.top
	return
	end
    else:
endcase

if plt then plotp,x,y,p

end

;***************************************************************
pro wdplot,xa,ya,getp=getp,setp=setp,nowd=nowd,xrange=xrange,yrange=yrange, $
	xtitle=xtitle,ytitle=ytitle,title=title,xform=xform,yform=yform, $
	incline=incline,xticklen=xticklen,yticklen=yticklen

;	getp	- return plotting parameters, sys-variables
;	setp	- plotting parameters, sys-variables
;	nowd	- if set, not display the range widget
;	incline - if set (1,2,,), then change line type with n# of y(*,n1,n2)
common wdplot,wdp,p,x,y

x=xa &	y=ya
nn=101	; step# of scaling
psys=!p &	xsys=!x &	ysys=!y

if keyword_set(setp) then begin
	p=setp
	!p=p.p
	!x=p.x
	!y=p.y
	if keyword_set(title) then !p.title=title
	plotp,xa,ya,p
endif else begin
	if keyword_set(title) then !p.title=title
	if not keyword_set(xrange) then xrange=[0,0]
	if not keyword_set(yrange) then yrange=[0,0]
	if not keyword_set(xtitle) then xtitle=''
	if not keyword_set(ytitle) then ytitle=''
	if not keyword_set(xform) then xform=''
	if not keyword_set(yform) then yform=''
	if not keyword_set(xticklen) then xticklen=0
	if not keyword_set(yticklen) then yticklen=0
	if not keyword_set(incline) then incline=0

	nx=n_elements(x)-1 &	yy=fltarr(nx)
	yy(0)=min(y) &	yy(nx-1)=max(y)
	plot,x,yy,/nodata,xtitle=xtitle,ytitle=ytitle,xrange=xrange,yrange=yrange, $
		xticklen=xticklen,yticklen=yticklen
	!x.range=!x.crange &	!y.range=!y.crange 	; <= do before oplot!
	!x.ticklen=xticklen &	!y.ticklen=yticklen
	!x.title=xtitle &	!y.title=ytitle
	xdd=!x.range(0)+[1,1+(!x.range(1)-!x.range(0))*0.01] &	ydd=!y.range(0)+[0,0]
	oplot,xdd,ydd,line=0
	;print,!x.crange,!y.crange
	imgsize,y,n0,n1,n2
	line=0
	for i=0,n1-1 do begin
		if incline eq 1 then line=i
		oplot,x,y(*,i),line=line
		oplot,xdd,ydd,line=line
	endfor
	for j=0,n2-1 do begin
		if incline eq 2 then line=j
		for i=0,n1-1 do begin
			if incline eq 1 then line=i
			oplot,x,y(*,i,j),line=line
			oplot,xdd,ydd,line=line
		endfor
	endfor
	val=findgen(nn)
	xval=!x.range(0)+(!x.range(1)-!x.range(0))/float(nn-1)*val
	yval=!y.range(0)+(!y.range(1)-!y.range(0))/float(nn-1)*val
	p={wdplot_prm, $
		p:	!p,	$
		x:	!x,	y:	!y,	$ ; sys variable
		xval:	xval,	yval:	yval,	$ ; values for slider
		xlog:	0,	ylog:	0,	$ ; if log-scale, set 1
		xform:	xform,	yform:	yform,	$ ; format for tickname
		incline:	incline	$ ; incriment line on this 
		}
	getp=p
endelse
if keyword_set(nowd) then return

base = WIDGET_BASE(title='wdplot', /column)
b1 = widget_base(base, /row, /frame )	
dmy=widget_label(b1,value='Y:',font=1)
dmy=min(abs(!y.range(0)-p.yval),iy1)
dmy=min(abs(!y.range(1)-p.yval),iy2)
wdymin=wd_sldbx(b1,name='min',values=string(p.yval),form=p.yform,iinit=iy1,/edit)
wdymax=wd_sldbx(b1,name='max',values=string(p.yval),form=p.yform,iinit=iy2,/edit)
b2 = widget_base(base, /row, /frame )	
dmy=widget_label(b2,value='X:',font=1)
dmy=min(abs(!x.range(0)-p.xval),ix1)
dmy=min(abs(!x.range(1)-p.xval),ix2)
wdxmin=wd_sldbx(b2,name='min',values=string(p.xval),form=p.xform,iinit=ix1,/edit)
wdxmax=wd_sldbx(b2,name='max',values=string(p.xval),form=p.xform,iinit=ix2,/edit)
bc=widget_base(base, /row )
wdp={wd_plot, $
	Xmax:	wdxmax,		Xmin:	wdxmin,	$
	Ymax:	wdymax,		Ymin:	wdymin,	$
	Gifsv:	0l,	$
	Quit:	0l	$
	}

wdp.Quit = widget_button(bc, value="Quit", uvalue = "Quit")

widget_control, base, /realize
XMANAGER, 'wdplot', base,/modal

p.p=!p &	p.x=!x &	p.y=!y
getp=p
setp=p
;print,p.y.range
;!p=psys &	!x=xsys &	!y=ysys

end
