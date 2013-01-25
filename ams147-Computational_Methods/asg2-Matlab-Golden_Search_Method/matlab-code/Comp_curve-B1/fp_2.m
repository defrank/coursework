function [y]=fp_2(x,c)
%
% This function calculates df(x)/dx for a given x and a parameter c.
%
h=1.0e-5;
y=(f(x+h,c)-f(x-h,c))/(2*h);
%
