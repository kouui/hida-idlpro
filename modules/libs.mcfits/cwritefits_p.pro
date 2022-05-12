; History
;    2021.09.02   u.k



;**************************************************************
pro cwritefits_p,imgs,p,file=file,s4pos=s4pos,rotp=rotp, $
                        time_arr=time_arr,dst_status=dst_status,trig_arr=trig_arr,frate_arr=frate_arr,threading=threading

    case size(imgs,/type) of
        12: img_type = "uint16"
        else: begin
            message, 'Unexpected imgs type encountered.'
            on_error, 2
        end
    endcase


    dll='Z:\Projects\cprog\VS2017\cfitsioIDL2\x64\Debug\cfitsioIDL2.dll'

    ;; append header
    err = call_external(dll,'add_header',"DATE_OBS", "string", p.date_obs)
    err = call_external(dll,'add_header',"DATE_OB2", "string", p.date_obs2)
    err = call_external(dll,'add_header',"TIMESYS",  "string", p.timesys)
    err = call_external(dll,'add_header',"OBSERVAT", "string", p.observat)
    err = call_external(dll,'add_header',"TELESCOP", "string", p.telescop)
    err = call_external(dll,'add_header',"CAMERA",   "string", p.camera)
    err = call_external(dll,'add_header',"EXP",      "float",  p.expo)
    err = call_external(dll,'add_header',"DATA_TYP", "string", p.data_typ)
    err = call_external(dll,'add_header',"VARTYPE",  "string", "UINT")
    err = call_external(dll,'add_header',"X0",       "int",    fix(p.RegionX))
    err = call_external(dll,'add_header',"Y0",       "int",    fix(p.RegionY))
    err = call_external(dll,'add_header',"BIN",      "int",    fix(p.bin))
    err = call_external(dll,'add_header',"TRIGMODE", "string", p.TrigMode)
    err = call_external(dll,'add_header',"TRIGPOL",  "string", p.TrigPol)
    err = call_external(dll,'add_header',"SPARSE",   "string", p.sparse)
    
    if n_elements(s4pos) ne 0 then begin    ; header information for S4 position , integer
        err = call_external(dll,'add_header',"S4POS",   "int", s4pos)
    endif 

    if keyword_set(rotp) then begin        ; header information for rotation period of rotating waveplate, float
        err = call_external(dll,'add_header',"ROTP",   "float", rotp)
    endif

    ;;append image extensions
    if keyword_set(dst_status) then begin  ;-- struct of dst status
        dstst = status2array(dst_status)
        case size(dstst,/type) of
            12: ext_type = "uint16"
            13: ext_type = "uint32"
            15: ext_type = "uint64"
            4 : ext_type = "float32"
            else: begin
	        message, 'Unexpected dst_status array data type encountered.'
	        on_error, 2
	    end
        endcase
        ext_name = "DST_STATUS"
        sext = size(dstst)
        err = call_external(dll,'add_image_extension',ext_name, ext_type, sext[0], sext[1:sext[0]], dstst)
    endif

    if keyword_set(time_arr) then begin  ;-- time info from FLIR cam.
        case size(time_arr,/type) of
            12: ext_type = "uint16"
            13: ext_type = "uint32"
            15: ext_type = "uint64"
            4 : ext_type = "float32"
            else: begin
	        message, 'Unexpected time_arr array data type encountered.'
	        on_error, 2
	    end
        endcase
        ext_name = "TIME_ARR"
        sext = size(time_arr)
        err = call_external(dll,'add_image_extension',ext_name, ext_type, sext[0], sext[1:sext[0]], time_arr)
    endif

    if keyword_set(trig_arr) then begin  
        case size(trig_arr,/type) of
            12: ext_type = "uint16"
            13: ext_type = "uint32"
            15: ext_type = "uint64"
            4 : ext_type = "float32"
            else: begin
	        message, 'Unexpected trig_arr array data type encountered.'
	        on_error, 2
	    end
        endcase
        ext_name = "TRIG_ARR"
        sext = size(trig_arr)
        err = call_external(dll,'add_image_extension',ext_name, ext_type, sext[0], sext[1:sext[0]], trig_arr)
    endif

    if keyword_set(frate_arr) then begin  
        case size(frate_arr,/type) of
            12: ext_type = "uint16"
            13: ext_type = "uint32"
            15: ext_type = "uint64"
            4 : ext_type = "float32"
            else: begin
	        message, 'Unexpected frate_arr array data type encountered.'
	        on_error, 2
	    end
        endcase
        ext_name = "FR_ARR"
        sext = size(frate_arr)
        err = call_external(dll,'add_image_extension',ext_name, ext_type, sext[0], sext[1:sext[0]], frate_arr)
    endif


    ;; write fits file
    sim = size(imgs)
    pthread = 0
    if keyword_set(threading) then pthread = 1 
    err = call_external(dll,'write_fits',file, sim[0], sim[1:sim[0]], imgs, pthread)
     

END

