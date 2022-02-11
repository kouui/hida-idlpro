function hybrid,Func,Din,Pin,Fmin=f1,Smin=Smin,Sfin=S,ITMAX=ITMAX, $
                Dint=D,Dmin=Dmin,Dmax=Dmax,silent=silent,flag=flag

;+
;Name:
;       HYBRID
;
;Purpose:
;       Minimizes an user-written function Func using the powell
;       hybrid method.
;
;Calling Sequence:
;       Pout=hybrid(Func,Din,Pin,[Fmin=Fmin],[Smin=Smin],[Sfin=Sfin], $
;                   [ITMAX=ITMAX],[Dint=D],[Dmin=Dmin],[Dmax=Dmax], $
;                   [/silent],[flag=flag])
;
;Input Parameters:
;       Func - A scalar string specifying the name of a user-supplied
;              IDL function.
;              Func should be acceptable m-elements vector and output
;              n-elements vector.
;              input (m-elements) -> Func -> output (n_elements) 
;       Din  - A n-elements vector data to which Func optimize.
;       Pin  - A m-elements vector specifying the starting point.
;
;Output Parameters:
;       Pout   - A n-elements vector minimizes Func.
;       Fmin   - The value of Func at the minimum-point Pout.
;       Sfin   - The error sum of squares at the minimum-point Pout.
;
;Keywords:
;       Smin    - The minimum limit of error sum of squares.
;                 If error sum of squares becames less than this
;                 value, the iterations finishs and return the result.
;                 The default is 1.0e-8.
;       ITMAX   - The maximum iterations allowed. The default is 200.
;       Dint    - The initial step size. The default is 1.0e0.
;       Dmin    - The minimum step size. The default is 1.0e-8.
;       Dmax    - The maximum step size. The default is 1.0e1.
;       /silent - Silent mode.
;       flag    - Condition of iteration ending.
;                 flag = 0 : Iteration reached ITMAX.
;                 flag = 1 : Iteration completed successfully.
;                 flag = 2 : The answer was (maybe) trapped in local minimum.
;
;History:
;       Written 09 Nov 2006, K. Otsuji
;-

if not n_elements(Smin)  then Smin =1.0d-8
if not n_elements(ITMAX) then ITMAX=200
if not n_elements(D)     then D    =1.0d0
if not n_elements(Dmin)  then Dmin =1.0d-8
if not n_elements(Dmax)  then Dmax =1.0d1

m=n_elements(Pin)               ;The number of parameters.
n=n_elements(call_function(Func,Pin)) ;The number of data.
w1=indgen(m)#replicate(1,m) $
  -replicate(1,m)#indgen(m) gt 0 ;Culculation matrix
w2=1-w1                         ;Culculation matrix

;==============================
;Culclation of A
;==============================
Dmat=identity(m,/DOUBLE)*D
A=dblarr(n,m)
for i=0,m-1 do $
  A(*,i)=[call_function(Func,Pin+Dmat(*,i))-call_function(Func,Pin)]/D

;==============================
;OMEGA / l / tau initialize
;==============================
OMEGA=identity(m,/DOUBLE)
l=m-indgen(m)-1
tau=1.0d0

;==============================
;Iteration begin
;==============================
P=Pin                           ;Parameter
flag=''                         ;Flag

for j=0,ITMAX-1 do begin
    f=call_function(Func,P)     ;Culculated data
    v=f-Din                     ;Residual error
    S=total(v^2)                ;Error sum of squares
    
;==============================
;Gauss-Newton method
;==============================
    qr,A,Q,R
    dxG=qrsolve(Q,R,-v)

;==============================
;Steepest descent method
;==============================
    b=-transpose(A)#v
    dxS=total(b^2)/total((A#b)^2)*b

;==============================
;D re-initialize
;==============================
    if j eq 0 then d=norm(dxS,lnorm=2)<Dmax>Dmin

;==============================
;Determination of dx
;==============================
    case 1 of
        norm(dxG,lnorm=2) le D :begin
            dx=dxG & flag='ii'  ;(ii)
            hybrid_sub,Func,P,dx,Din,A,f,f1,v,S, $
              Smin,D,Dmin,Dmax,tau,sub_flag
        end
        else :begin
            case 1 of
                norm(dxS,lnorm=2) lt D :begin
                    ddx=dxG-dxS
                    c1=total(ddx^2)
                    c2=total(ddx*dxS)
                    c3=total(dxS^2)-D^2
                    k=(-c2+sqrt(c2^2-c1*c3))/c1
                    dx=k*dxG+(1-k)*dxS
                end
                norm(dxS,lnorm=2) ge D :dx=D*dxS/norm(dxS,lnorm=2)
            endcase
            
;==============================
;Independence of dx
;==============================
            case 1 of
                l(0) le 2*m-2 :begin
                    flag='i'    ;(i)
                    hybrid_sub,Func,P,dx,Din,A,f,f1,v,S, $
                      Smin,D,Dmin,Dmax,tau,sub_flag
                end
                abs(total(OMEGA(*,0)*dx)/norm(dx,lnorm=2)) gt 0.5 :begin
                    flag='iii'  ;(iii)
                    hybrid_sub,Func,P,dx,Din,A,f,f1,v,S, $
                      Smin,D,Dmin,Dmax,tau,sub_flag
                end
                else :flag='iv' ;(iv)
            endcase
        end
    endcase

    case sub_flag of
        0:
        1:begin
            if not isvalid(silent) then $
              print,'% Iteration completed successfully.'
            return,P
        end
        2:begin
            if not isvalid(silent) then $
              print,'% The answer was (maybe) trapped in local minimum.'
            return,P
        end
    endcase

;==============================
;OMEGA l revision
;==============================
    case 1 of
        flag eq 'iv' or norm(dx,lnorm=2) lt dmin :begin ;(iv),(v)
            if flag ne 'iv' then flag='v'
            dx=dmin*OMEGA(*,0)
            f1=call_function(Func,P+dx)
            v1=f1-Din
            S1=total(v1^2)
            OMEGA=shift(OMEGA,[0,-1])
            l(0:m-2>0)=l(1<(m-1):m-1)+1 & l(m-1)=0
        end
        else :begin             ;(i),(ii),(iii)
            alpha=transpose(OMEGA)#dx/norm(dx,lnorm=2)
            lmin=min(where(transpose(alpha)^2#w1 le 0.75))
            l(0:lmin-1>0)=l(0:lmin-1>0)+1
            l(lmin :m-2>lmin)=l(lmin+1<(m-1):m-1)+1
            l(m-1)=0
            OMEGA=OMEGA(*,[lmin,where(indgen(m) ne lmin)])
            alpha=alpha([lmin,where(indgen(m) ne lmin)])
            OMEGA(*,0:m-2>0)= $
              [diag_matrix(transpose(alpha(0:m-2>0)^2)#w2(0:m-2>0,0:m-2>0))] $
              ##OMEGA(*,1<(m-1):m-1) $
              -diag_matrix(alpha(1<(m-1):m-1)) $
              ##[(diag_matrix(alpha(0:m-2>0))##OMEGA(*,0:m-2>0))#w2(0:m-2>0,0:m-2>0)]
            OMEGA(*,m-1)=dx
            OMEGA=OMEGA#diag_matrix(1/sqrt(total(OMEGA^2,1)))
        end
    endcase
    
;==============================
;A adjustment
;==============================
    A=A+(f1-f-A#dx)#transpose(dx)/total(dx^2)

;==============================
;Iteration end
;==============================
endfor

if not isvalid(silent) then print,'% Iteration reached ITMAX.'

return,P

end




