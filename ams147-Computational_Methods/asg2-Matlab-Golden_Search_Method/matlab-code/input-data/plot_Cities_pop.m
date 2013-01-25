%
% This code reads in data from Cities_pop.txt and plots the data
% Cities_pop.txt contains population data of 6 cities from 1850 -1990
% These six cities are: Atlanta, New York, Boston,
% Chicago, San Francisco, Los Angeles
%
clear
figure(2)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load -ascii Cities_pop.txt
t=Cities_pop(:,1);
y_ATL=1.0e3*Cities_pop(:,2);
y_NY =1.0e3*Cities_pop(:,3);
y_BOS=1.0e3*Cities_pop(:,4);
y_CHI=1.0e3*Cities_pop(:,5);
y_SF =1.0e3*Cities_pop(:,6);
y_LA=1.0e3*Cities_pop(:,7);
%
semilogy(t, y_ATL,'b-s')
hold on
semilogy(t, y_NY, 'b-s','markerfacecolor','b')
semilogy(t, y_BOS,'k-o')
semilogy(t, y_CHI,'k-o','markerfacecolor','k')
semilogy(t, y_SF, 'r-d')
semilogy(t, y_LA, 'r-d','markerfacecolor','r')
set(gca,'fontsize',14)
axis([1840,2000,1.0e3,2.0e7])
set(gca,'xtick',[1850:20:1990])
set(gca,'ytick',10.^[3:7])
xlabel('Year')
ylabel('Populations')
title('Populations of 6 cities from 1850-1990')
h1=legend('Atlanta','New York','Boston','Chicago',...
'San Francisco','Los Angeles',4);
set(h1,'fontsize',12)
%
%
