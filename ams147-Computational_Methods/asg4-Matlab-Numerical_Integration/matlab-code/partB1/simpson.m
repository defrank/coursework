% This code uses the composite Simpson rule to calculate
% int_{a}^{b} f(x) dx.
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
x2=zeros(M,N(M));
y2=zeros(M,N(M));
S=zeros(M);
errS=zeros(M);
for i=1:M,
    h(i)=(b-a)/N(i);
    for j=0:N(i),
        x(i,j+1)=a+j*h(i);
        y(i,j+1)=f(x(i,j+1));
    end
    for j=0:N(i)-1,
        x2(i,j+1)=a+j*h(i)+h(i)/2;
        y2(i,j+1)=f(x2(i,j+1));
    end
    S(i)=(y(i,1)+y(i,N(i)+1)+2*sum(y(i,2:N(i)))+4*sum(y2(i,1:N(i))))*h(i)/6;
    errS(i)=abs(S(i)-I);
end
%
save simp.mat S errS
% display values
for i=1:M,
    disp(' ')
    disp(['  The numerical result by the composite Simpson'])
    disp(['  rule with N = ',num2str(N(i)),' is'])
    disp(['        S = ',num2str(S(i),'%16.8e'),'.'])
    disp(['  The error is ',num2str(errS(i),'%16.8e'),'.'])
    disp(' ')
end