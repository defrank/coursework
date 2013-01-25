% This code uses the Euler method to solve y'=-sin(y)+2*t*sin(4*t)
% from t=0 to t=5. Then it plots the numerical solution
%
clear
figure(3)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
y0=1;
h=0.1;
%
n=5/h;
t=[0:n]*h;
y=zeros(1,n+1);
y(1)=y0;
%
for j=1:n,
  y(j+1)=y(j)+h*(-sin(y(j))+2*t(j)*sin(4*t(j)));
end
% h=.0008
h2=0.0008;
%
n2=5/h2;
t2=[0:n2]*h2;
y2=zeros(1,n2+1);
y2(1)=y0;
%
for j=1:n2,
  y2(j+1)=y2(j)+h2*(-sin(y2(j))+2*t2(j)*sin(4*t2(j)));
end
%
plot(t,y,'m-','linewidth',2.0)
hold on
plot(t2,y2,'r--','linewidth',2.0)
%
set(gca,'fontsize',14)
xlabel('Time, t')
ylabel('Numerical Approximation, y')
title('Euler Method Numerical Approximation')
legend('y(t), h=0.1','y(t), h=0.0008')