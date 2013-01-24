% Derek Frank, dmfrank@ucsc.edu
% HW1: PartB (2)
% Due: 1/20/10
%
% Plot f(x,y) = sin(x^2+y^2)*exp(-sqrt(x^2+y^2))
%   for x in [-3,3] and y in [-3,3].
%
clear;
clf;
%
m=30;
n=30;
dx=6/(n-1);
dy=6/(m-1);
x1=[-3:dx:3];
y1=[-3:dy:3];
%
[x,y]=meshgrid(x1,y1);
%
f=sin(x.^2+y.^2).*exp(-sqrt(x.^2+y.^2)) ;
%
h=surf(x,y,f);
set(h,'facecolor','interp')
set(h,'edgecolor',[0.5,0.5,0.5])
%
axis([-3.0,3.0,-3.0,3.0,-.4,.4])
set(gca,'xtick',[-2:1:3])
set(gca,'ytick',[-2:1:2])
set(gca,'ztick',[-.2:.2:.4])
set(gca,'fontsize',14)
xlabel('x')
ylabel('y')
zlabel('f(x,y)')
%
