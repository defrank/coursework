function [D]=gp_2(r,c)
%
% This function calculates df(x)/dx for a given x and a parameter c,
% using a second finite difference method.
%
D=zeros(2,2);
h=1.0e-5;
D(:,1)=(g(r+[h,0]',c)-g(r-[h,0]',c))/(2*h);
D(:,2)=(g(r+[0,h]',c)-g(r-[0,h]',c))/(2*h);
%
