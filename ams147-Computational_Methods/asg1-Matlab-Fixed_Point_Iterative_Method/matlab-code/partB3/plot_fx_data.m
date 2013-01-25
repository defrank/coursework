% Derek Frank, dmfrank@ucsc.edu
% HW1: PartB (3)
% Due: 1/20/10
%
% This code reads in data from data1.txt and plots 
% f(x) = exp(-c*x)*cos(2*x) along with the data from data1.txt
%
clear
figure(3)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load -ascii data1.txt
x=data1(:,1);
y=data1(:,2);
%
% fit the data to f(x)
x2=[0:.2:10];
C=.2295528077;
y2=exp(-C.*x2).*cos(2.*x2);
%
plot(x,y,'bo','markerfacecolor','b')
hold on
plot(x2,y2,'k-')
set(gca,'fontsize',14)
axis([-1.0,11.0,-1.0,1.5])
set(gca,'xtick',[0:1:11])
set(gca,'ytick',[-.5:.5:1.5])
xlabel('x')
ylabel('y')
title('Plotting data')
h1=legend('data','fitting curve',4);
set(h1,'fontsize',12)
%