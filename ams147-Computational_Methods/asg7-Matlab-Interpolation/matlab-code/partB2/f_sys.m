function [z]=f_sys(w,t)
% This function calculates f_sys(w,t)
%
z=zeros(1,2);
theta=w(1);
v=w(2);
z(1)=v;
z(2)=-sin(theta);