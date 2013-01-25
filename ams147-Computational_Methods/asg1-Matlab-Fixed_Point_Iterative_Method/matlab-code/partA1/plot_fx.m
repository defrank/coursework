%
% Plot f1(x) = sin(x) and f2(x) = exp(cos(x)^2) for x in [-4,4]
%
clear;
clf;
%
n=81;
dx=8/(n-1);
x=[-4:dx:4];
f1=sin(x);
f2=exp(cos(x).^2);
%
plot(x,f1,'b-','linewidth',2.0)
hold on
plot(x,f2,'k-o')
axis([-4.0,4.0,-1.4,3.8])
set(gca,'xtick',[-4:1:4])
set(gca,'ytick',[-1:1:3])
set(gca,'fontsize',14)
xlabel('x')
ylabel('f(x)')
h1=legend('f1(x)=sin(x)','f2(x)=exp(cos(x)^2)');
set(h1,'fontsize',12)
%
