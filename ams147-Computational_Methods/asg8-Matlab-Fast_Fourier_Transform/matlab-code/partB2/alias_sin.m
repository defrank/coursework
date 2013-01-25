% This code plots different sin functions
% g1(t) = sin(t)        t = 2*pi*[0:N]/N, N = 500
% g2(t) = sin(21t)      t = 2*pi*[0:N]/N, N = 500
% g3(t) = sin(21t)      t = 2*pi*[0:N]/N, N = 20
%
clear
figure(5)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
N1=500; N2=20;
t1=2*pi*[0:N1]/N1; t2=2*pi*[0:N2]/N2;
g1=sin(t1); g2=sin(21*t1); g3=sin(21*t2);
%
plot(t1,g1,'b-','linewidth',2.0)
hold on
plot(t1,g2,'r--','linewidth',2.0)
hold on
plot(t2,g3,'bs','markerfacecolor','b')
axis([-.1,2*pi+.1,-1.1,1.1])
set(gca,'xtick',[0:pi/2:7])
set(gca,'ytick',[-1:.5:1])
set(gca,'fontsize',12)
xlabel('t')
ylabel('f(t)')
h1=legend('sin(t), t = 2*pi*[0:N]/N, N = 500','sint(21t), t = 2*pi*[0:N]/N, N = 500','sin(21t), t = 2*pi*[0:N]/N, N = 20',2);
set(h1,'fontsize',12)
title('Aliasing effect of sin(t) functions')