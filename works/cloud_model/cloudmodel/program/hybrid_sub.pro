pro hybrid_sub,Func,P,dx,Din,A,f,f1,v,S,Smin,D,Dmin,Dmax,tau,sub_flag

;+
;Name:
;       HYBRID_SUB
;
;Purpose:
;       Subroutine of HYBRID.PRO.
;
;History:
;       Written 09 Nov 2006, K. Otsuji
;-       

f1=call_function(Func,P+dx)
v1=f1-Din
S1=total(v1^2)
dS=S1-S
dS_=total([v+A#dx]^2)-total(v^2)

case 1 of
    dS_ lt 0. :begin
        r=dS/dS_
        
;==============================
;d adjustment
;==============================
        case 1 of
            r lt 0.1 :begin
                D=D/2.>Dmin
                tau=1.0d0
            end
            r ge 0.1 :begin
                df=f1-[f+A#dx]
                Sp=total(abs(v1*df))
                Ss=total(df^2)
                lambda=sqrt(1+(0.1-r)*dS_/(Sp+sqrt(Sp^2+(0.1-r)*dS_*Ss)))
                
                mu=min([2,lambda,tau])
                tau=lambda/mu
                D=min([mu*D,Dmax])>Dmin
            end
        endcase        
        P=P+dx*(dS le 0)
        sub_flag=S1 le Smin
    end
    else :begin
        P=P+dx*(dS le 0)
        sub_flag=2              ;Maybe local minimum.
    end
endcase

end
