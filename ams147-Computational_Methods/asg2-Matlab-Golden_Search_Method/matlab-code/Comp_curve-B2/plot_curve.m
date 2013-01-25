%
% Consider the non-linear equation fitting function
% f(x)=exp(-c*x)*cos(2*x).  With a given x, "c" is a parameter in
% the equation.  The code "calc_data.m" calculates
% sqrt(sum of (f(x(i))-y(i))^2), the distance between the data
% given in "datal.txt" and the fitting function for "c" in [0,1]
% and stores the data in data1.mat.  This code loads in data1.mat
% and plots the distance as a function of "c".
%
clear
figure(3)
clf
axes('position',[0.15,0.13,0.75,0.75])
%
load data1.mat
plot(c_v, y_v,'linewidth',2.0)
axis([0,1,0,4])
set(gca,'xtick',[0:.2:1])
set(gca,'ytick',[1:1:4])
set(gca,'fontsize',14)
xlabel('c')
ylabel('f(c)')
title('The distance between "data1.txt" and the fitting function')
%

