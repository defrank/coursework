%
% This code reads in data from US_pop.txt and plots the data
% US_pop.txt contains US population data from 1790 -1990
%
clear
figure(1)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load -ascii US_pop.txt
t=US_pop(:,1);
y=US_pop(:,2);
%
semilogy(t,y,'b-s','markerfacecolor','b')
set(gca,'fontsize',14)
axis([1780,2000,1.0e6,5.0e8])
set(gca,'xtick',[1800:30:1990])
set(gca,'ytick',10.^[6:8])
xlabel('Year')
ylabel('US population')
title('US population from 1790-1990')
%
%
