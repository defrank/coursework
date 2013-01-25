% This code uses the composite Simpson rule to calculate
% g(s)=int_{0}^{2} fp(x)dx, s in [0,4].
%
clear
figure(3)
clf
axes('position',[0.15,0.13,0.75,0.75])
%
a=0; b=4;
N=256;
hx=2/N;
hs=b/N;
% first calculate g(s)
x=a+[0:N]*hx;
x2=a+[0:N-1]*hx+hx/2;
s=a+[0:N]*hs;
s2=a+[0:N-1]*hs+hs/2;
%
y=zeros(N+1);
y2=zeros(N);
g=zeros(N+1);
for i=1:N+1,
    for j=1:N+1,
        y(j)=fp(x(j),s(i));
        if j~=N+1,
            y2(j)=fp(x2(j),s(i));
        end
    end
    g(i)=(y(1)+y(N+1)+2*sum(y(2:N))+4*sum(y2(1:N)))*hx/6;
end
z=zeros(N);
z2=zeros(N);
g2=zeros(N);
for i=1:N,
    for j=1:N,
        z(j)=fp(x(j),s2(i));
        z2(j)=fp(x2(j),s2(i));
    end
    g2(i)=(z(1)+z(N)+2*sum(z(2:N-1))+4*sum(z2(1:N-1)))*hs/6;
end
%
% now calculate G(t)
t=[0:hs:4];
G=zeros(N+1);
for i=1:N+1,
    G(i)=(g(1)+g(N+1)+2*sum(g(2:N))+4*sum(g2(1:N)))*hs/6;
end
% plot and display results
for i=1:N+1,
    plot(t(i), G(i),'g*')
    hold on
    disp(' ')
    disp(['  The numerical result by the composite Simpson'])
    disp(['  rule with N = ',num2str(N),' is'])
    disp(['        S = ',num2str(G(i),'%16.8e'),'.'])
    disp(' ')
end
axis([0,4,4,4.5])
set(gca, 'ytick', [4.1:.1:4.5])
set(gca,'fontsize',14)
xlabel('t=[0,4]')
ylabel('G(t)=int_{0}^{t}g(s)')
title('The Numerical Estimation of G(t) using Composite Simpson Rule')
legend('int_{0}^{t}int_{0}^{2}(sqrt(1+exp(-3*cos(s*x)))-1.5)dx')