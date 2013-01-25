% This code estimates the error of the fourth order
% numerical differentiation method and plots it as
% a function of the spatial step.  As well, the order
% of accuracy is estimated at t=25 and plotted.
%
clear
figure(3)
clf reset

load data_sRK4
%
err1=abs(w1(nstep1+1,1)-w2(nstep1+1,1))/(1-0.5^4)+1.0e-16;
err2=abs(w2(nstep2+1,1)-w3(nstep2+1,1))/(1-0.5^4)+1.0e-16;
err3=abs(w3(nstep3+1,1)-w4(nstep3+1,1))/(1-0.5^4)+1.0e-16;
err4=abs(w4(nstep4+1,1)-w5(nstep4+1,1))/(1-0.5^4)+1.0e-16;
err5=abs(w5(nstep5+1,1)-w6(nstep5+1,1))/(1-0.5^4)+1.0e-16;
err6=abs(w6(nstep6+1,1)-w7(nstep6+1,1))/(1-0.5^4)+1.0e-16;
%
axes('position',[0.18,0.56,0.74,0.36])
loglog(h1, err1,'kv','markerfacecolor','k')
hold on
loglog(h2, err2,'cp','Markerfacecolor','c')
hold on
loglog(h3, err3,'bo','Markerfacecolor','b')
hold on
loglog(h4, err4,'g*','Markerfacecolor','g')
hold on
loglog(h5, err5,'mo','Markerfacecolor','m')
hold on
loglog(h6, err6,'rs','Markerfacecolor','r')
hold on
%
%axis([10*e-3,10,10*e(0.01),10*e(0.02)])
set(gca,'fontsize',12)
%set(gca,'xtick',10.^[-5:-1])
%set(gca,'ytick',[1:10^(.02)])
%xlabel('Spatial step size, h')
ylabel('Estimated error, E_{t=25}(h)')
title('Fourth order method error at t=25')
legend('h=0.2','h=0.2*2^{-1}','h=0.2*2^{-2}','h=0.2*2^{-3}','h=0.2*2^{-4}','h=0.2*2^{-5}')
% estimate order of accuracy
p11=abs(w1(nstep1-1,1)-w1(nstep1,1));
p12=abs(w1(nstep1,1)-w1(nstep1+1,1));
p1=log2(p11/p12);
p21=abs(w2(nstep2-1,1)-w2(nstep2,1));
p22=abs(w2(nstep2,1)-w2(nstep2+1,1));
p2=log2(p21/p22);
p31=abs(w3(nstep3-1,1)-w3(nstep3,1));
p32=abs(w3(nstep3,1)-w3(nstep3+1,1));
p3=log2(p31/p32);
p41=abs(w4(nstep4-1,1)-w4(nstep4,1));
p42=abs(w4(nstep4,1)-w4(nstep4+1,1));
p4=log2(p41/p42);
p51=abs(w5(nstep5-1,1)-w5(nstep5,1));
p52=abs(w5(nstep5,1)-w5(nstep5+1,1));
p5=log2(p51/p52);
p61=abs(w6(nstep6-1,1)-w6(nstep6,1));
p62=abs(w6(nstep6,1)-w6(nstep6+1,1));
p6=log2(p61/p62);
p71=abs(w7(nstep7-1,1)-w7(nstep7,1));
p72=abs(w7(nstep7,1)-w7(nstep7+1,1));
p7=log2(p71/p72);
%
axes('position',[0.18,0.09,0.74,0.36])
semilogx(h1,p1,'kv','markerfacecolor','k')
hold on
semilogx(h2,p2,'cp','Markerfacecolor','c')
hold on
semilogx(h3,p3,'bo','Markerfacecolor','b')
hold on
semilogx(h4,p4,'g*','Markerfacecolor','g')
hold on
semilogx(h5,p5,'mo','Markerfacecolor','m')
hold on
semilogx(h6,p6,'rs','Markerfacecolor','r')
hold on
semilogx(h7,p7,'yd','Markerfacecolor','y')
set(gca,'fontsize',12)
xlabel('Spatial step size, h')
ylabel('Order of accuracy, p_{t=25}')
title('Estimated Order of Accuracy')
legend('h=0.2','h=0.2*2^{-1}','h=0.2*2^{-2}','h=0.2*2^{-3}','h=0.2*2^{-4}','h=0.2*2^{-5}','h=0.2*2^{-6}')
%
disp(['p = [',num2str(p1),']'])
disp(['p = [',num2str(p2),']'])
disp(['p = [',num2str(p3),']'])
disp(['p = [',num2str(p4),']'])
disp(['p = [',num2str(p5),']'])
disp(['p = [',num2str(p6),']'])
disp(['p = [',num2str(p7),']'])