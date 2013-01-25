function [yp]=fp(x,s)
% This function calculates fp(x,s).
%
yp=sqrt(1+exp(-3*cos(s*x)))-1.5;