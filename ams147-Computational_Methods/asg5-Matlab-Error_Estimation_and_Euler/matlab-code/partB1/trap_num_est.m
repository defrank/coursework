function [T]=trap_num_est(h,N)
% This code uses the composite Trapezoidal rule to calculate
% int_{a}^{b} f(x) dx.
%
a=0.0; b=2.0;
nsize=10;
%
x=a+[0:N]*h;
y=f(x);
T=(y(1)+y(N+1)+2*sum(y(2:N)))*h/2;