function [err_est, t1]=est_err(h)
% This code uses the Euler method to solve y'=-sin(y)+2*t*sin(4*t)
% with time step h=0.1 and h=0.05. Then it estimates the error
% and plots the estimated error as a function of time. 
%
y0=1;
%h=.1;
%
% Run with h
%
n=5/h;
t=[0:n]*h;
y=zeros(1,n+1);
y(1)=y0;
for j=1:n,
  y(j+1)=y(j)+h*(-sin(y(j))+2*t(j)*sin(4*t(j)));
end
h1=h;
n1=n;
t1=t;
y1=y;
%
% Run with h/2
%
h=h/2;
n=5/h;
t=[0:n]*h;
y=zeros(1,n+1);
y(1)=y0;
for j=1:n,
  y(j+1)=y(j)+h*(-sin(y(j))+2*t(j)*sin(4*t(j)));
end
h2=h;
n2=n;
t2=t;
y2=y;
%
% Error at t=5
err_t5=abs(y1(n1+1)-y2(n2+1))/(1-0.5);
disp(' ')
disp(['  The estimated error for h = ',num2str(h1),'  at t = 5 is'])
disp(['        Error = ',num2str(err_t5,'%16.8e'),'.'])
disp(' ')
%
% Error as a function of time
err_est=abs(y1-y2(1:2:n2+1))/(1-0.5);