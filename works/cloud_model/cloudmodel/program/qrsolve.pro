function qrsolve,Q,R,y

;+
;NAME:
;       QRSOLVE
;
;Purpose:
;       Solving a simultaneous equation A#x=y using QR factorizing.
;
;Calling Sequence:
;       x=qrsolve(Q,R,y)
;
;Input Parameters:
;       Q - An orthogonal NxM matrix such that TRANSPOSE(Q)#Q equals
;           the identity matrix.
;       R - An lower triangular MxM matrix.
;           A=Q#R
;	    Q and R are obtained by QR procedure.
;	y - Right hand side of the simultaneous equation.
;
;Output Parameters:
;	x - Solution of the simultaneous equation.
;
;History:
;       Written 21 Nov 2006, K. Otsuji
;-

m=(size(Q))(2)
x=dblarr(m)

z=transpose(Q)#y

for i=m-1,0,-1 do x(i)=(z(i) $
	-(i ne m-1)*total(R(i,i+1<(m-1):m-1)*x(i+1<(m-1):m-1)))/R(i,i)

return,x

end