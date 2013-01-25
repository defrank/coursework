function [r, n]=newton(funct_name, deriv_name, c, x0, tol)
%
% This function finds a root of f(x) = 0 using Newton's method.
%
% Input:
%	funct_name: the name of the .m file for calculating the function f(x)
%	deriv_name: the name of the .m file for calculating df(x)/dx
%	c:  a parameter in functions "f" and "fp"
%	x0: the starting point for Newton's method
%	tol: the error tolerance
% Output:
%	r:  the root found
%	n:  the number of iterations
%
err=1.0;
n=0;
%
while(err > tol),
  n=n+1;
  f_x0=feval(funct_name,x0,c);
  fp_x0=feval(deriv_name,x0,c);
  x1=x0-f_x0/fp_x0;
  err=abs(x1-x0);
  x0=x1;
end
%
r=x0;
%
