clear
%
load data_sRK4c
td=t;
yd=wc(1:nstep+1,1);
%
a=6;
b=9;
tol=1.0e-10;
n=0;
%
g=(sqrt(5)-1)/2;
r1=a+(b-a)*(1-g);
f1=dd(r1,td,yd);
r2=a+(b-a)*g;
f2=dd(r2,td,yd);
%
while (b-a) > tol,
  n=n+1;
  if f1 < f2,
    b=r2;
    r2=r1;
    f2=f1;
    r1=a+(b-a)*(1-g);
    f1=dd(r1,td,yd);
  else
    a=r1;
    r1=r2;
    f1=f2;
    r2=a+(b-a)*g;
    f2=dd(r2,td,yd);
  end
end
T=(a+b)/2