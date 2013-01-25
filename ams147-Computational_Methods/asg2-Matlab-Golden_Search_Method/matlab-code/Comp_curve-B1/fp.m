function [y]=fp(x,c)
%
% This function calculates df(x)/dx for a given x and a parameter c.
%
y=-1.*(exp(-x)./(x.^2))-1.*(exp(-x)./x)+1./(x.^2)+2;
%
