%
% Consider the non-linear equation (exp(-x)-1)/x+(x-c-2*sin(c))=0.
% Here "c" is a parameter in the equation. The root of the equation varies
% with "c" and thus the root is a function of "c". 
% This code calculates the root for "c" in [-10,0] and stores the data 
% in data1.mat. Later on the data is used in "plot_curve.m" to plot 
% the root as a function of "c".
%
clear
%
c_v=[-10:0.2:0];
nc=size(c_v,2);
r_v=zeros(1,nc);
tol=1.0e-10;
r=1;
%
for i=1:nc,
  c=c_v(i);
  [r, n]=newton('f', 'fp_2', c, r, tol);
  r_v(i)=r;
end
%
save data1 c_v r_v
%
