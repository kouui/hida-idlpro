;; read *.xml config file for mmdst_adjust
;; 2022.02.07  u.k.  initialized
FUNCTION MMDST_ADJUST_XML_VALUE, item

    tp = item['%type']
    text = item['#text']

    case tp of 
    'int': begin
        return, fix(text)
    end
    'float': begin
        return, float(text)
    end
    'string': begin
        return, text
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

FUNCTION MMDST_ADJUST_XML_READ, path

    data = XML_PARSE(path)
    
    return, data
END

FUNCTION MMDST_ADJUST_XML_SELECT, data, key

    item = data['config', key]
    value = MMDST_ADJUST_XML_VALUE(item)
    return, value
END

PRO TEST_MMDSTXML
    path = '/tmp_mnt/home/kouui/idlpro/works/dstpol/tmp/He1083.20220110.mmdst_adjust.xml'
    data = MMDST_ADJUST_XML_READ(path)
    keys = ['wl0','telpos','bin','savefile','iedgeL','icentL','iedgeC','icentC','islit1','islit2','iobs1','iobs2','error','niter','fix_sc','fix_th_vs','corr_Icrtk','xpos']
    nkey = n_elements(keys)
    for i=0, nkey-1 do begin 
        print, '[', keys[i], ']'
        help, MMDST_ADJUST_XML_SELECT(data, keys[i]) 
    endfor
END
