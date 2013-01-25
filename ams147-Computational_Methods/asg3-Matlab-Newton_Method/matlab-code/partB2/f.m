function [y]=f(x,c)
%
% This function calculates f(x) for a given x and a parameter c.
%
u=x(1);
v=x(2);
y=zeros(2,1);
y(1)=exp(u)-cos(v)+u-v-c;
y(2)=exp(v)+sin(u)+v+u;
%
