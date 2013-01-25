function [z]=g(r,c)
%
% r=(u,v)'
% z=g(r,c)=(g1(u,v,c), g2(u,v,c))'
%
% DO NOT MODIFY THIS FILE!!!!!
%
N=128;
da=pi/N;
a=[0:N]*da;
[x,y]=meshgrid(a,a);
%
alpha=c;
mu=0.6;
alpha0=1;
E=1;
r1=r(1);
r3=r(2);
%
h1=sin(y).*cos(x);
h3=cos(y);
g0=sin(y).*exp(alpha*r1*h1+(alpha*r3+mu*E)*h3+0.5*alpha0*E^2*h3.^2);
%
g=g0;
g(:,1)=0.5*(g(:,1)+g(:,N+1));
u1=sum(g(:,1:N),2)*da;
%
g=h1.*g0;
g(:,1)=0.5*(g(:,1)+g(:,N+1));
u2=sum(g(:,1:N),2)*da;
%
g=h3.*g0;
g(:,1)=0.5*(g(:,1)+g(:,N+1));
u3=sum(g(:,1:N),2)*da;
%
u=[u1, u2, u3];
u(1,:)=0.5*(u(1,:)+u(N+1,:));
v1=sum(u(1:N,:),1)*da;
v2=sum(u(1:2:N,:),1)*2*da;
v3=sum(u(1:4:N,:),1)*4*da;
v4=sum(u(1:8:N,:),1)*8*da;
v5=sum(u(1:16:N,:),1)*16*da;
v6=sum(u(1:32:N,:),1)*32*da;
v1=(4*v1-v2)/3;
v2=(4*v2-v3)/3;
v3=(4*v3-v4)/3;
v4=(4*v4-v5)/3;
v5=(4*v5-v6)/3;
v1=(16*v1-v2)/15;
v2=(16*v2-v3)/15;
v3=(16*v3-v4)/15;
v4=(16*v4-v5)/15;
v1=(64*v1-v2)/63;
v2=(64*v2-v3)/63;
v3=(64*v3-v4)/63;
v1=(256*v1-v2)/255;
v2=(256*v2-v3)/255;
v1=(512*v1-v2)/511;
err=(v1-v2)/norm(v1);
%
z=[v1(2)/v1(1)-r1; v1(3)/v1(1)-r3];
%
