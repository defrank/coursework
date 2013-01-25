%
% Plot f1(x) = sin(x) and f2(x) = exp(cos(x)^2) for x in [-4,4]
%
clear;
clf;
%
n=81;
x=zeros(1,n);
f1=zeros(1,n);
f2=zeros(1,n);
dx=8/(n-1);
%
for i=1:n,
   x(i)=-4+(i-1)*dx;
   f1(i)=sin(x(i));
   f2(i)=exp(cos(x(i))^2);
end
%
plot(x,f1)
hold on
plot(x,f2)
%
