% This code uses the composite Simpson rule to calculate
% g(s)=int_{0}^{2} fp(x)dx, s in [0,4].
%
clear
figure(2)
clf
axes('position',[0.15,0.13,0.75,0.75])
%
a=0;
N=256;
h=2/N;
ds=4/(N); s=[0:ds:4];
%
x=a+[0:N]*h;
x2=a+[0:N-1]*h+h/2;
%
y=zeros(N+1);
y2=zeros(N);
S=zeros(N+1);
for i=1:N+1,
    for j=1:N+1,
        y(j)=fp(x(j),s(i));
        if j~=N+1,
            y2(j)=fp(x2(j),s(i));
        end
    end
    S(i)=(y(1)+y(N+1)+2*sum(y(2:N))+4*sum(y2(1:N)))*h/6;
end
%
for i=1:N+1,
    plot(s(i), S(i),'bo')
    hold on
    disp(' ')
    disp(['  The numerical result by the composite Simpson'])
    disp(['  rule with N = ',num2str(N),' is'])
    disp(['        S = ',num2str(S(i),'%16.8e'),'.'])
    disp(' ')
end
axis([0,4,-1.5,2.5])
set(gca,'fontsize',14)
xlabel('s=[0,4]')
ylabel('g(s)')
title('The Numerical Estimation of g(s) using Composite Simpson Rule')
legend('int_{0}^{2}(sqrt(1+exp(-3*cos(s*x)))-1.5)dx')