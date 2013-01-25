% This code implements the classical four stage fourth order
% Runge Kutta method to solve the two body problem. After
% the calculation, it saves the workspace to a data file.
%
clear
%
m=12;
w0=[0.97000436, -0.24308753, -0.97000436, 0.24308753, 0, 0,...
    0.93240737/2, 0.86473146/2, 0.93240737/2, 0.86473146/2,...
    -0.93240737, -0.86473146];
% h
cf=2^9;
h=1/cf;
nstep=10*cf;
%
w=zeros(nstep+1,m);
t=zeros(nstep+1,1);
t(1)=0;
w(1,1:m)=w0;
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
    k(i,1:m)=h*feval('f_sys', w(j,1:m)+c(i,1:i-1)*k(1:i-1,1:m), t(j)+d(i)*h);
  end
  w(j+1,1:m)=w(j,1:m)+b*k;
  t(j+1)=t(j)+h;
end
%
save data_3body