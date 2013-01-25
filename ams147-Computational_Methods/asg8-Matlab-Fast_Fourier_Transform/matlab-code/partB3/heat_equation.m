%
% This code uses the Fourier transform to solve
% the heat equation
%
clear
figure(1);
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
L=2*pi; L1=2; L2=1; L3=2; L4=2*pi-5;
c0=4*pi^2/L^2;
%
N=512;
dx=L/N; %dx1=L1/(2/2*pi)*N; dx2=L2/(2/2*pi)*N;
%dx3=L3/(2/2*pi)*N; dx4=L4/(2/2*pi)*N;
x1=[0:N-1]*dx;
y=fft(u0);
ind=[0:N/2,-N/2+1:-1];
%
t=0.01;
y2_1=y(1).*exp(-c0*ind.^2*t);y2_2=y(2).*exp(-c0*ind.^2*t);
y2_3=y(3).*exp(-c0*ind.^2*t);y2_4=y(4).*exp(-c0*ind.^2*t);
u_01_1=real(ifft(y2_1)); u_01_2=real(ifft(y2_2));
u_01_3=real(ifft(y2_3)); u_01_4=real(ifft(y2_4));
%
t=0.1;
y2=y(1).*exp(-c0*ind.^2*t);
u_05=real(ifft(y2));
%
plot(x1,u0(1),'k-','linewidth',1.0)
hold on
plot(x2,u0(2),'k-','linewidth',1.0)
hold on
plot(x3,u0(3),'k-','linewidth',1.0)
hold on
plot(x4,u0(4),'k-','linewidth',1.0)
hold on
plot(x1,u_01_1,'b-','linewidth',1.0)
hold on
plot(x2,u_01_2,'b-','linewidth',1.0)
hold on
plot(x3,u_01_3,'b-','linewidth',1.0)
hold on
plot(x4,u_01_4,'b-','linewidth',1.0)
hold on
plot(x1,u_05,'r-','linewidth',1.0)
%
%axis([0,6.5,-0.5,7.5])
set(gca,'fontsize',14)
set(gca,'xtick',[0:1:6])
set(gca,'ytick',[0:2:6])
xlabel('x')
ylabel('u')
legend('t = 0','t = 0.1','t = 0.5',1)
%
%
