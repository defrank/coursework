function [T]=simp_num_est(h, N)
% This code uses the composite Simpson rule to calculate
% int_{a}^{b} f(x) dx.
%
a=0.0; b=2.0;
%
x=a+[0:N]*h;
y=f(x);
x2=a+[0:N-1]*h+h/2;
y2=f(x2);
T=(y(1)+y(N+1)+2*sum(y(2:N))+4*sum(y2))*h/6;