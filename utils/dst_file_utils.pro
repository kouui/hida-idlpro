
;************************************************************************
; FUNCTION
; NAME       : dst_filename2time
; PURPOSE    : 
;    convert filename of dst fits file to a datetime struct
; CALLING SEQUENCE :
;      dst_filename2time(fname)
; INPUTS     :
;      fname   - string, filename of dst fits file
;  MODIFICATION HISTORY :
;        k.u. 2021.12.05
;************************************************************************

FUNCTION dst_filename2time, fname

    datetime = {datetime_struct,$
                 year      : 0U,$
                 month     : 0U,$
                 day       : 0U,$
                 hour      : 0U,$
                 minute    : 0U,$
                 second    : 0U,$
                 milisecond: 0U $
                      }
    words = STRSPLIT(fname, '/', /EXTRACT)
    nw = SIZE(words, /N_ELEMENTS)
    fn = words[nw-1]
    words = STRSPLIT(fn, '.', /EXTRACT)
    nw = SIZE(words, /N_ELEMENTS)
    datetime.milisecond = UINT(words[nw-2])
    words = STRSPLIT(words[0], '_', /EXTRACT)
    nw = SIZE(words, /N_ELEMENTS)
    ;;words_tmp = STRSPLIT(words[0], '_', /EXTRACT)
    if (n_elements(words) le 1) then throw_error, "["+fn+"]"+" does not contain '_'"
    if (n_elements(words) gt 3) then throw_error, "["+fn+"]"+" contains more than 2 '_'"
    if (n_elements(words) eq 2) then   ymd = words[nw-1] else ymd = words[nw-2]
    hms = words[nw-1]
    datetime.year = UINT(STRMID(ymd,0,4))
    datetime.month = UINT(STRMID(ymd,4,2))
    datetime.day = UINT(STRMID(ymd,6,2))
    datetime.hour = UINT(STRMID(hms,0,2))
    datetime.minute = UINT(STRMID(hms,2,2))
    datetime.second = UINT(STRMID(hms,4,2))

    return, datetime
END

;************************************************************************
; FUNCTION
; NAME       : dst_time2seconds
; PURPOSE    : 
;    given a datetime struct, calculate the total passed seconds in that day
; CALLING SEQUENCE :
;      dst_time2seconds(datetime)
; INPUTS     :
;      fname   - datetime, datetime struct
;  MODIFICATION HISTORY :
;        k.u. 2021.12.05
;************************************************************************
FUNCTION dst_time2seconds, datetime
    
    total_seconds = 0L
    total_seconds += LONG(datetime.hour) * 60 * 60
    total_seconds += LONG(datetime.minute) * 60
    total_seconds += LONG(datetime.second) 
    return, total_seconds

END

FUNCTION dst_time2str, datetime
    s = string(FIX(datetime.year),format="%04d")
    s += "-" + string(FIX(datetime.month),format="%02d")
    s += "-" + string(FIX(datetime.day),format="%02d")
    s += " " + string(FIX(datetime.hour),format="%02d")
    s += ":" + string(FIX(datetime.minute),format="%02d")
    s += ":" + string(FIX(datetime.second),format="%02d")
    return, s
END

FUNCTION str_startswith, text, prefix
    n_prefix = STRLEN(prefix)
    n_text = STRLEN(text)
    if (n_prefix gt n_text) then THROW_ERROR, "prefix longer than main text??"
    pre_text = STRMID(text,0,n_prefix)
    return, (pre_text eq prefix) 
END

FUNCTION path_basename, text
    words = STRSPLIT(text, '/', /EXTRACT)
    nw = SIZE(words, /N_ELEMENTS)
    fn = words[nw-1]
    return, fn
END

FUNCTION dst_scan_seconds_range, files, prefixs, offset_head, offset_tail
    SETDEFAULTVALUE, offset_head, -10
    SETDEFAULTVALUE, offset_tail, 0
    
    n_prefix = SIZE(prefixs, /N_ELEMENTS)
    n_file = SIZE(files, /N_ELEMENTS)
         
    ranges = LONARR(2,n_prefix)
    for j=0,n_prefix-1 do begin
        pre = prefixs[j]
        for i=0,n_file-1 do begin
            fn = path_basename(files[i])
            fname = files[i]
            ;words = STRSPLIT(fname, '/', /EXTRACT)
            ;nw = SIZE(words, /N_ELEMENTS)
            ;fn = words[nw-1]
            if (~str_startswith(fn, pre)) then CONTINUE
            sec = dst_time2seconds(dst_filename2time(fname))
            if (ranges[0,j] eq 0) then begin
                ranges[0,j] = sec+offset_head
            endif
            if (ranges[0,j] gt sec+offset_head) then ranges[0,j] = sec+offset_head
            if (ranges[1,j] lt sec+offset_tail) then ranges[1,j] = sec+offset_tail
        endfor
    endfor
    return, ranges
END


FUNCTION dst_scan_indicator, files, prefixs, ranges

    n_prefix = SIZE(prefixs, /N_ELEMENTS)
    n_file = SIZE(files, /N_ELEMENTS)
    indicators = UINTARR(n_file)
    for i=0,n_file-1 do begin
        ;fn = path_basename(files[i])
        sec = dst_time2seconds(dst_filename2time(files[i]))
        for j=0,n_prefix-1 do begin
            if ((ranges[0,j] le sec) && (ranges[1,j] ge sec)) then begin
                indicators[i] = j
                break
            endif
            indicators[i] = j 
        endfor
    endfor

    return, indicators
END

FUNCTION file_select_by_time, files, hour, minute, second

    ;;-- decide the head of scan
    ;hms = [14,6,12] ; hour, minute, second
    ;hms_sec = LONG(hms[0])*60*60 + LONG(hms[1])*60 + LONG(hms[2])
    hms_sec = LONG(hour)*60*60 + LONG(minute)*60 + LONG(second)
    nF = SIZE(files, /N_ELEMENTS)
    for j0=0,nf-1 do begin
        file_sec = dst_time2seconds(dst_filename2time(files[j0]))
	  if (file_sec ge hms_sec) then break
    endfor
    ;UNDEFINE, hms
    ;UNDEFINE, hms_sec
    ;UNDEFINE, file_sec
    ;;-- ended : decide the head of scan
    return, j0
END

FUNCTION offset_hhmmss_array, hhmmss, offset

    total_seconds = hhmmss[0] * 60 * 60 + hhmmss[1] * 60 + hhmmss[2] + offset
    hhmmss_new = [0,0,0]
    hhmmss_new[0] = total_seconds / 3600
    total_seconds = total_seconds - hhmmss_new[0] * 3600
    if hhmmss_new[0] > 24 then hhmmss_new[0] = hhmmss_new[0] - 24
    hhmmss_new[1] = total_seconds / 60
    total_seconds = total_seconds - hhmmss_new[1] * 60
    hhmmss_new[2] = total_seconds
    return, hhmmss_new
END
