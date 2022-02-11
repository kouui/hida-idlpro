

FUNCTION MMDST_ADJUST_SKIP_LINE, line

    ;; skip empty line
    if STRLEN(line) eq 0 then return, 1b
    ;; skip comment line
    if STRMID(line,0,1) eq '#' then return, 1b

    return, 0b

END

FUNCTION MMDST_ADJUST_GET_VALUE, item

    ;tp = item['%type']
    ;text = item['#text']
    
    ;key = item.key
    tp  = item.type
    text= item.text

    case tp of 
    'int': begin
        return, fix(text)
        
    end
    'float': begin
        return, float(text)
        ;value = float(text)
    end
    'string': begin
        return, text
        ;value = text
    end
    'byte': begin
        case text of
        '1' : return,1b
        '0' : return,0b
        else : throw_error, 'undefined xml item (type=byte) with text=', text
        endcase
    end
    else: throw_error, 'undefined xml item with type=', tp
    endcase
END

FUNCTION MMDST_ADJUST_TXT_SELECT,data, key

    nitem = n_elements(data)
    for i=0, nitem-1 do begin
        item = data[i]
        if item.key eq key then return, MMDST_ADJUST_GET_VALUE(item)
    endfor
    throw_error, "unable to find configuration item with key=",key
END


FUNCTION MMDST_ADJUST_TXT_READ, file

if keyword_set(file) then begin
    OPENR, lun, file, /GET_LUN
    ; Read one line at a time, saving the result into array
    data = []
    line = ''
    WHILE NOT EOF(lun) DO BEGIN
        READF, lun, line
        line = STRTRIM(line, 2)
        if MMDST_ADJUST_SKIP_LINE(line) then continue
        print, line
        words = STRSPLIT(line,',',/EXTRACT)
        nw = n_elements(words)
        if not nw eq 3 then throw_error, "n_element of line not eq 3: ", line
        words1 = []
        for i=0,nw-1 do words1 = [words1,STRTRIM(words[i], 2)]
        ;data = [[data], [words1]]
        data = [data, {mmdst_adjust_text_item, key:words1[0],type:words1[1],text:words1[2]}]
    ENDWHILE
    ;print, data
    ; Close the file and free the file unit
    FREE_LUN, lun

    iedgeL = MMDST_ADJUST_TXT_SELECT(data, 'iedgeL')
    icentL = MMDST_ADJUST_TXT_SELECT(data, 'icentL')
    iedgeC = MMDST_ADJUST_TXT_SELECT(data, 'iedgeC')
    icentC = MMDST_ADJUST_TXT_SELECT(data, 'icentC')
    islit1 = MMDST_ADJUST_TXT_SELECT(data, 'islit1')
    islit2 = MMDST_ADJUST_TXT_SELECT(data, 'islit2')
    iobs1  = MMDST_ADJUST_TXT_SELECT(data, 'iobs1')
    iobs2  = MMDST_ADJUST_TXT_SELECT(data, 'iobs2')
    error  = MMDST_ADJUST_TXT_SELECT(data, 'error')
    niter  = MMDST_ADJUST_TXT_SELECT(data, 'niter')
    fix_sc = MMDST_ADJUST_TXT_SELECT(data, 'fix_sc')
    fix_th_vs = MMDST_ADJUST_TXT_SELECT(data, 'fix_th_vs')
    corr_Icrtk= MMDST_ADJUST_TXT_SELECT(data, 'corr_Icrtk')
    
    wl0 = MMDST_ADJUST_TXT_SELECT(data, 'wl0')
    bin = MMDST_ADJUST_TXT_SELECT(data, 'bin')
    savfolder = MMDST_ADJUST_TXT_SELECT(data, 'savfolder')
    xpos = MMDST_ADJUST_TXT_SELECT(data, 'xpos')
endif else begin
    iedgeL = -1
    icentL = -1
    iedgeC = -1
    icentC = -1
    islit1 = -1
    islit2 = -1
    iobs1  = -1
    iobs2  = -1
    error  = 2E-4
    niter  = 20
    fix_sc = 1b
    fix_th_vs = 0b
    corr_Icrtk= 1b
    
    wl0 = -1
    bin = 1
    savfolder = ''
    xpos = -1
endelse
    sconf = {symfit_struct,$
    iedgeL : iedgeL, $
    icentL : icentL, $
    iedgeC : iedgeC, $
    icentC : icentC, $
    islit1 : islit1, $
    islit2 : islit2, $
    iobs1  : iobs1, $
    iobs2  : iobs2, $
    error  : error, $
    niter  : niter, $
    fix_sc : fix_sc, $
    fix_th_vs : fix_th_vs, $
    corr_Icrtk: corr_Icrtk $
    }

    conf = {mmdst_adjust_configuration, $
    wl0      : wl0, $
    bin      : bin, $
    savfolder: savfolder, $
    xpos     : xpos, $
    sconf    : sconf $ 
    }
    
    return, conf
END

FUNCTION MMDST_ADJUST_TXT_TEST

    file = '/home/kouui/idlpro/mmdst/He1083.20220110.mmdst_adjust.conf'
    conf = MMDST_ADJUST_TXT_READ(file)

    return, conf
END