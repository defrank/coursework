function [y]=f(x,c)
%
% This function calculates f(x) for a given x and a parameter c.
%
y=(exp(-x)-1)./x+(x-c-2.*sin(c));
%
