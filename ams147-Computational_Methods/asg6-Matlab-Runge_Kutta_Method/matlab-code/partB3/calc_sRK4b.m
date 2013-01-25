% This code implements the classical four stage fourth order
% Runge Kutta method to solve an ODE system. After the
% calculation, it saves the workspace to a data file.
%
clear
%
m=2;
w0b=[-2, 4];
h=0.05;
nstep=25/h;
%
wb=zeros(nstep+1,m);
tb=zeros(nstep+1,1);
tb(1)=0;
wb(1,1:m)=w0b;
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
    k(i,1:m)=h*f_sys(wb(j,1:m)+c(i,1:i-1)*k(1:i-1,1:m), tb(j)+d(i)*h);
  end
  wb(j+1,1:m)=wb(j,1:m)+b*k;
  tb(j+1)=tb(j)+h;
end
%
save data_sRK4b