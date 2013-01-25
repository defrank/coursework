% This code uses the composite Trapezoidal rule to calculate
% int_{a}^{b} f(x) dx. f(x)=sinx
% The error is calculated using the exact solution.
%
a=0; b=2;
I=cos(a)-cos(b);
%
N=2.^[1:1:10];
M=10;
h=zeros(M);
x=zeros(M,N(M)+1);
y=zeros(M,N(M)+1);
T=zeros(M);
errT=zeros(M);
for i=1:M,
    h(i)=(b-a)/N(i);
    for j=0:N(i),
        x(i,j+1)=a+j*h(i);
        y(i,j+1)=f(x(i,j+1));
    end
    T(i)=(y(i,1)+y(i,N(i)+1)+2*sum(y(i,2:N(i))))*h(i)/2;
    errT(i)=abs(T(i)-I);
end
%
save trap.mat T errT N M
% display values
for i=1:M,
    disp(' ')
    disp(['  The numerical result by the composite Trapezoidal'])
    disp(['  rule with N = ',num2str(N(i)),' is'])
    disp(['        T = ',num2str(T(i),'%16.8e'),'.'])
    disp(['  The error is ',num2str(errT(i),'%16.8e'),'.'])
    disp(' ')
end