clear
figure(5)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load data_d_T
%
plot(Ta,da,'linewidth',2.0)
axis([4.9,8,-.5,11])
set(gca,'fontsize',14)
set(gca,'xtick',[5:8])
set(gca,'ytick',[0:2:12])
xlabel('T')
ylabel('dd(T)')
title('Period, T, for position of Body 1')