
;************************************************************************
; procedure
; NAME       : throw_error
; PURPOSE    : 
;    throw error with an text discription
; CALLING SEQUENCE :
;      throw_error, text
; INPUTS     :
;      text   - string, text discription
;  MODIFICATION HISTORY :
;        k.u. 2021.09.20
;************************************************************************

PRO throw_error, text
    ON_ERROR, 2   ; stop in caller
    MESSAGE, LEVEL=-1, text
END
