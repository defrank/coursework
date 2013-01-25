% This code uses the composite Simpson rule to calculate
% int_{a}^{b} cos(sx)f2(x) dx.
% The error is calculated using the exact solution.
%
clear
clear
figure(5)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
a=0; b=10;
%
s=[0:.03:3];
sizes=101;
%
N=512;
h=(b-a)/N;
x=a+[0:N]*h; x2=a+[0:N-1]*h+h/2;
%
%g=zeros(sizes);
%
disp(' ')
disp(['  The numerical result by the composite Simpson'])
disp(['  rule with N = ',num2str(N),' is'])
%
for i=1:sizes,
    y=f(x,s(i));
    y2=f(x2,s(i));
    g=(y(1)+y(N+1)+2*sum(y(2:N))+4*sum(y2))*h/6;
    disp(['        s = ',num2str(s(i),'%16.8e'),'.'])
    disp(['        g(s) = ',num2str(g,'%16.8e'),'.'])
    disp(' ')
    % plot
    plot(s(i),g,'bo','markerfacecolor','b')
    hold on
end
%
set(gca,'fontsize',12)
axis([0,3,0,2])
set(gca,'xtick',[0:1:3])
%set(gca,'ytick',[-0.5:0.5:1])
title('Composite Simpson Rule calculation of g(s)')
xlabel('s')
ylabel('g(s)')
h1=legend('g(s) = int_{0}^{10} cos(s*x)*f(x) dx');
set(h1,'fontsize',12)