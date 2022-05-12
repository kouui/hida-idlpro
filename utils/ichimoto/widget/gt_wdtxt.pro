; +
; NAME       : gt_wdtxt.pro (function)
; PURPOSE :
;    get string from text widget
; CATEGORY :
;	idlpro/util
; CALLING SEQUENCE :
;	string=gt_wdtxt()
; INPUTS : none
; OUTPUT : string
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;	K.I. '99/12/01
;-
function gt_wdtxt,evid
	widget_control, evid, get_value=value, set_value=''
	widget_control, evid, set_value=value(0)
	return,	value(0)
end
