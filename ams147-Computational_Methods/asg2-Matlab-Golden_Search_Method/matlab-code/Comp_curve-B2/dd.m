function [dist]=dd(c,x,y)
%
% This function calculates the distance between the data
% and the function f(x)=exp(-c*x)*cos(2x)
%
fx=exp(-c*x).*cos(2*x);
dist=norm(fx-y);
%
