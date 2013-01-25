function [dx]=f(t,x)
#  size(x);
  dx=zeros(2,1);
  dx(1)=5*x(1)+10*x(2);
  dx(2)=-x(1)-x(2);
endfunction
