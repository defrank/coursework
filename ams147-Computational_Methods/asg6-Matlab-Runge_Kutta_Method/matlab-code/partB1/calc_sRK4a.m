% This code implements the classical four stage fourth order
% Runge Kutta method to solve an ODE system. After the
% calculation, it saves the workspace to a data file.
%
clear
%
m=2;
w0a=[.1, 0];
h=0.05;
nstep=25/h;
%
wa=zeros(nstep+1,m);
t=zeros(nstep+1,1);
t(1)=0;
wa(1,1:m)=w0a;
%
p=4;
d=[0,   1/2, 1/2, 1  ];
c=[0,   0,   0,   0  ;
   1/2, 0,   0,   0  ;
   0,   1/2, 0,   0  ;
   0,   0,   1,   0  ]; 
b=[1/6, 1/3, 1/3, 1/6];
k=zeros(p,m);
%
for j=1:nstep,
  for i=1:p,
    k(i,1:m)=h*f_sys(wa(j,1:m)+c(i,1:i-1)*k(1:i-1,1:m), t(j)+d(i)*h);
  end
  wa(j+1,1:m)=wa(j,1:m)+b*k;
  t(j+1)=t(j)+h;
end
%
save data_sRK4a