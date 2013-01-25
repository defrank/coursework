function [y]=f(x,s)
% This function calculates f(x,s).
%
load -ascii data4.txt
xd=data4(:,1);
yd=data4(:,2);
%
%x=[0:0.05:10];
y_sp=spline(xd,yd,x);
y=cos(s*x).*y_sp;