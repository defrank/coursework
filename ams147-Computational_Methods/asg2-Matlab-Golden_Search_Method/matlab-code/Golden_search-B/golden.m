%
% This code first reads in data (x, y) from "data1.txt."
% Then it uses the golden search method to find the value of c
% such that the distance between the data and the function
% f(x)=exp(-c*x)*cos(2*x) is minimized.
% Finally, it plots the data along with the best fit.
%
clear
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load -ascii data1.txt
x=data1(:,1);
y=data1(:,2);
%
a=0;
b=2;
tol=1.0e-10;
n=0;
%
g=(sqrt(5)-1)/2;
r1=a+(b-a)*(1-g);
f1=dd(r1,x,y);
r2=a+(b-a)*g;
f2=dd(r2,x,y);
%
while (b-a) > tol,
  n=n+1;
  if f1 < f2,
    b=r2;
    r2=r1;
    f2=f1;
    r1=a+(b-a)*(1-g);
    f1=dd(r1,x,y);
  else
    a=r1;
    r1=r2;
    f1=f2;
    r2=a+(b-a)*g;
    f2=dd(r2,x,y);
  end
end
c0=(a+b)/2;
%
fx=exp(-c0*x).*cos(2*x);
plot(x,fx,'b-','linewidth',2.0)
hold on
plot(x,y,'ko','markerfacecolor',[0.5,0.5,0.5])
axis([0,10,-.8,1.1])
set(gca,'xtick',[2:2:10])
set(gca,'ytick',[-.6:0.2:1.1])
set(gca,'fontsize',14)
xlabel('x')
ylabel('y')
title(['Best fit: c = ',num2str(c0,8)])
h1=legend('f(x)=exp(-c*x)*cos(2*x)','data');
set(h1,'fontsize',12)
%
disp('  ')
disp(['  The function attains a minimum at c0 = ',num2str(c0,'%24.16e'),'.'])
disp(['  It takes n = ',num2str(n),' iterations to reach err <= ',num2str(tol),'.'])
disp('  ')
%
