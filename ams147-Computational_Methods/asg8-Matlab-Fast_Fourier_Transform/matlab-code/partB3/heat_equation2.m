% This code uses the Fourier transform to solve
% the heat equation
%
clear
figure(6);
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
L=2*pi;
c0=4*pi^2/L^2;
%
N=512;
dx=L/N;
% N = [0,162],[163,243],[244,406],[407,511]
x=[0:N-1]*dx;
u0=[0,1,-1,0];
y=fft(u0);
ind1=[0:162/2,-162/2+1:-1];
ind2=[163/2:243/2,-243/2+1:163/2-1];
ind3=[244/2:406/2,-406/2+1:244/2-1];
ind4=[407/2:511/2,-511/2+1:407/2-1];
%
t=0.01;
y2=y(1)*exp(-c0*ind1.^2*t);
u_001_1=real(ifft(y2));
y2=y(2)*exp(-c0*ind2.^2*t);
u_001_2=real(ifft(y2));
y2=y(3)*exp(-c0*ind3.^2*t);
u_001_3=real(ifft(y2));
y2=y(4)*exp(-c0*ind4.^2*t);
u_001_4=real(ifft(y2));
%
t=0.1;
y2=y(1)*exp(-c0*ind1.^2*t);
u_01_1=real(ifft(y2));
y2=y(2)*exp(-c0*ind2.^2*t);
u_01_2=real(ifft(y2));
y2=y(3)*exp(-c0*ind3.^2*t);
u_01_3=real(ifft(y2));
y2=y(4)*exp(-c0*ind4.^2*t);
u_01_4=real(ifft(y2));
%
plot(x(1:163),u0(1),'ks')
hold on
plot(x(164:244),u0(2),'ks')
hold on
plot(x(245:407),u0(3),'ks')
hold on
plot(x(408:N),u0(4),'ks')
hold on
%
plot(x(1:162),u_001_1,'b-','linewidth',2.0)
hold on
plot(x(1:243),u_001_2,'b-','linewidth',2.0)
hold on
plot(x(1:406),u_001_3,'b-','linewidth',2.0)
hold on
plot(x(1:N-1),u_001_4,'b-','linewidth',2.0)
hold on
%
plot(x(1:162),u_01_1,'r-','linewidth',1.0)
hold on
plot(x(1:243),u_01_2,'r-','linewidth',1.0)
hold on
plot(x(1:406),u_01_3,'r-','linewidth',1.0)
hold on
plot(x(1:N-1),u_01_4,'r-','linewidth',1.0)
%
axis([-.2,6.5,-1.1,1.1])
set(gca,'fontsize',12)
set(gca,'xtick',[0:1:6])
set(gca,'ytick',[-1:.5:1])
xlabel('x')
ylabel('u')
legend('black is t = 0','blue is t = 0.01','red is t = 0.1')
title('Fast Fourier Transform on the heat equation')