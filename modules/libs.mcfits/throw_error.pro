;****************************************************
;
; NAME        : throw_error (procedure)
; PURPOSE     : throw error with an text description
; CALLING SEQUENCE :
;               throw_error, text
; INPUTS      :
;         text - error message, string
; HISTORY     :
;         k.u.  2021.09.20
;****************************************************

PRO throw_error, text
    ON_ERROR, 2   ; stop in caller
    MESSAGE, LEVEL=-1, text

END