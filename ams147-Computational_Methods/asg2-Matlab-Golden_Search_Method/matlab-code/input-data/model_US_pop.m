%
% This code first reads in data from US_pop.txt.
% Then it fits the data to the logistic population model
% and plots the data and the fitting curve.
%
% US_pop.txt contains US population data from 1790 -1990
%
clear
figure(3)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load -ascii US_pop.txt
t=US_pop(:,1);
y=US_pop(:,2);
%
% fit the data to the logistic population model
t2=[1790:1:2100];
A=0.03;
B=1.0e-10;
C=0.25e-6;
y2=1./(B/A+C*exp(-A*(t2-1790)));
%
semilogy(t,y,'bo','markerfacecolor','b')
hold on
semilogy(t2,y2,'k-')
set(gca,'fontsize',14)
axis([1780,2100,1.0e6,5.0e8])
set(gca,'xtick',[1800:40:2100])
set(gca,'ytick',10.^[6:8])
xlabel('Year')
ylabel('US population')
title('Fitting the logistic model to US population')
h1=legend('data','fitting curve',4);
set(h1,'fontsize',12)
%
%
