;; to compute the th_offset for dstpol for GOLDEYE and ORCA
;	2022.05.08	k.i.,u.k.  get_th_offset()

;********************************************************************
function get_th_offset,camera,rotp,expo,bin,cam_IF=cam_IF
;  rotp	- sec
;  expo	- sec

; empirical th_offset, 24.3 from Pol.data, 0.4 pol. offset from sunspot-V
if not keyword_set(cam_IF) then cam_IF = 'CamLink'
case camera of
	'GOLDEYE': begin
		deadtime = 3.32*0.001 
		th_offset0 = 0.5*(expo - deadtime) *360./rotp + 24.3 + 0.4  ; deg.  
		end
	'ORCA4' : begin	; experiment with USB connection
		obsbin = bin
		case obsbin of
		    1: begin
			case cam_IF of
			    'USB':     th_offset0 = (0.5*expo - 0.01) *360./rotp *4.37 + 23.5 + 0.4	; @ 587nm
			    'CamLink': th_offset0 = (0.5*expo - 0.006) *360./rotp *3.438 + 23.5 + 0.4 	; @ 587nm
			endcase
			end
		    2: th_offset0 = (0.5*expo - 0.003) *360./rotp *2.98 + 23.5 + 0.4	; @ 587nm
		endcase
		end
endcase

return,th_offset0

end
