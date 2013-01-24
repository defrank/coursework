% Derek Frank, dmfrank@ucsc.edu
% HW1: PartB (1)
% Due: 1/20/10
%
% Plot r1 = f1(b) = (-b+sqrt(b^2-4*c))/2) and 
%      r2 = f2(b) = (-b-sqrt(b^2-4*c))/2) for b in [2,3]
%
clear;
clf;
%
n=100;
db=1/(n-1);
b=[2:db:3];
c=1;
r1=(-b+sqrt((b.^2)-4*c))/2;
r2=(-b-sqrt((b.^2)-4*c))/2;
%
plot(b,r1,'b-','linewidth',2.0)
hold on
plot(b,r2,'k-o')
axis([2.0,3.0,-3.0,1.0])
set(gca,'xtick',[2.2:.2:3])
set(gca,'ytick',[-2:1:1])
set(gca,'fontsize',14)
xlabel('b')
ylabel('f(b)')
h1=legend('r1(b)=(-b+sqrt(b^2-4*c))/2)','r2(b)=(-b-sqrt(b^2-4*c))/2)');
set(h1,'fontsize',12)
%
