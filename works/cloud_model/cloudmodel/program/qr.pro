pro qr,A,Q,R

;+
;NAME:
;       QR
;
;Purpose:
;	QR factorizing of matrix using modification of Gram-Schmidt
;	method.
;
;Calling Sequence:
;	qr,A,Q,R
;
;Input Parameters:
;	A - A NxM matrix to be factored (N ge M). 
;
;Output Parameters:
;	Q - An orthogonal NxM matrix such that TRANSPOSE(Q)#Q equals
;	    the identity matrix.
;	R - An lower triangular MxM matrix.
;	    A=Q#R
;
;History:
;       Written 21 Nov 2006, K. Otsuji
;-

m=(size(A))(2)
n=(size(A))(1)

Q=double(A)
R=dblarr(m,m)

for i=0,m-1 do begin
    R(i,*)=transpose(Q(*,i))#Q/norm(Q(*,i),lnorm=2)
    Q(*,i)=Q(*,i)/norm(Q(*,i),lnorm=2)
    if i lt m-1 then Q(*,i+1:m-1)=Q(*,i+1:m-1)-Q(*,i)#R(i,i+1:m-1)
endfor

end
