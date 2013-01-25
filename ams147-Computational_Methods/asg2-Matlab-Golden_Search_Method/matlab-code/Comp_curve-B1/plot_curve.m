%
% Consider the non-linear equation (exp(-x)-1)/x+(x-c-2*sin(c))=0.
% Here "c" is a parameter in the equation. The root of the equation varies
% with "c" and thus the root is a function of "c". 
% The code "calc_data.m" calculates the root for "c" in [-10,0] and 
% stores the data in data1.mat. 
% This code loads in data1.mat and plots the root as a function of "c".
%
clear
clf
axes('position',[0.15,0.13,0.75,0.75])
%
load data1.mat
plot(c_v, r_v,'linewidth',2.0)
axis([-10,0,-3.5,1.0])
set(gca,'xtick',[-9:2:0])
set(gca,'ytick',[-3:1:1])
set(gca,'fontsize',14)
xlabel('c')
ylabel('r(c)')
title('The root of (exp(-x)-1)/x+(x-c-2*sin(c))=0')
%

