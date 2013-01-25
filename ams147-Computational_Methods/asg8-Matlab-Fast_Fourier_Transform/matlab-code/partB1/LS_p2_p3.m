% This code fits g(x) = b1*g1(x)+b2*g2(x)+b3*g3(x) to the data where
% g1(x) = 1
% g2(x) = x
% g3(x) = x^2
% g4(x) = x^3
%
clear
figure(4)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load data6.txt
x=data6(:,1);
y=data6(:,2);
%
g1=ones(size(x));
g2=x;
g3=x.^2;
g4=x.^3;
G2=[g1, g2, g3];
G3=[g1, g2, g3, g4];
%
A2=G2'*G2;
A3=G3'*G3;
G2y=G2'*y;
G3y=G3'*y;
b2=A2\G2y;
b3=A3\G3y;
%
xp=[0:0.01:1];
g_2=b2(1)+b2(2)*xp+b2(3)*xp.^2;
g_3=b3(1)+b3(2)*xp+b3(3)*xp.^2+b3(4)*xp.^3;
plot(xp,g_2,'b-','linewidth',2.0)
hold on
plot(xp,g_3,'r-','linewidth',2.0)
hold on
plot(x,y,'ko','markerfacecolor',[0.5,0.5,0.5])
axis([0,1,-1.0,3.5])
set(gca,'xtick',[0:0.2:1])
set(gca,'ytick',[-1:1:3])
set(gca,'fontsize',14)
xlabel('x')
ylabel('y')
h1=legend('p2(x), polynomial of degree 2','p3(x), polynomial of degree 3','data',2);
set(h1,'fontsize',12)
title('Least square fitting with polynomials of degree 2 and 3')