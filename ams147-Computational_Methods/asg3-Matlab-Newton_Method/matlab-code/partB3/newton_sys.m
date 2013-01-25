function [ru, rv, n]=newton_sys(funct_name, deriv_name, c, u, v, tol)
%
% This function finds a root of g(x) = 0 using Newton's method.
%
% Input:
%	funct_name: the name of the .m file for calculating the function g(r)
%	deriv_name: the name of the .m file for calculating dg(r)/dr
%	c:  a parameter in functions "g" and "gp_2"
%	u, v: the starting point for Newton's method
%	tol: the error tolerance
% Output:
%	r:  the root found
%	n:  the number of iterations
%
err=1.0;
n=0;
x0=[u v]';
%
while(err > tol),
  n=n+1;
  f_x0=feval(funct_name,x0,c);
  fp_x0=feval(deriv_name,x0,c);
  Dx=-fp_x0\f_x0;
  x0=x0+Dx;
  err=norm(Dx);
end
%
ru=x0(1);
rv=x0(2);
%
