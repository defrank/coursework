%
% Plot f(x,y) = sin(x)*cos(0.5*y) for x in [-3,3] and y in [-4,4].
%
clear;
clf;
%
m=31;
n=31;
x=zeros(m,n);
y=zeros(m,n);
f=zeros(m,n);
dx=6/(n-1);
dy=8/(m-1);
%
for i=1:m,
   for j=1:n,
      x(i,j)=-3+(j-1)*dx;
      y(i,j)=-4+(i-1)*dy;
      f(i,j)=sin(x(i,j))*cos(0.5*y(i,j));
   end
end
%
surf(x,y,f)
%
