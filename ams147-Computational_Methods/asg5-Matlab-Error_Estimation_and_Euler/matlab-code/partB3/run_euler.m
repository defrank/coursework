% Calculate and plot the error estimation for h=.1 and h=.0008
%
clear
figure(4)
clf reset
[y t1]=est_err(.1);
[y2 t2]=est_err(.0008);
%
axes('position',[0.18,0.56,0.74,0.36])
plot(t1,y,'k--','linewidth', 2.0)
hold on
plot(t2,y2,'g-','linewidth',2.0)
legend('Estimated error, h=0.1','Estimated error, h=0.0008')
set(gca,'fontsize',14)
ylabel('Error')
title('Estimated Error with Euler method')
%
axes('position',[0.18,0.09,0.74,0.36])
plot(t2,y2,'g-','linewidth',2.0)
set(gca,'fontsize',14)
xlabel('Time, t')
ylabel('Error')