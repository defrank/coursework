function [D]=fp(x,c)
%
% This function calculates df(x)/dx for a given x and a parameter c.
%
u=x(1);
v=x(2);
D=zeros(2,2);
D(1,1)=exp(u)+1;
D(1,2)=sin(v)-1;
D(2,1)=cos(u)+1;
D(2,2)=exp(v)+1;
%
