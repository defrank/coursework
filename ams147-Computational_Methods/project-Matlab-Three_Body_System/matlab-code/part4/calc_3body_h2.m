% This code implements the classical four stage fourth order
% Runge Kutta method to solve the two body problem. After
% the calculation, it saves the workspace to a data file.
%
clear
%
load data_3body
% h/2
cf2=2*cf;
h2=1/cf2;
nstep2=10*cf2;
%
w2=zeros(nstep+1,m);
t2=zeros(nstep+1,1);
t2(1)=0;
w2(1,1:m)=w0;
%
k2=zeros(p,m);
%
for j=1:nstep2,
  for i=1:p,
    k2(i,1:m)=h2*feval('f_sys', w2(j,1:m)+c(i,1:i-1)*k2(1:i-1,1:m), t2(j)+d(i)*h2);
  end
  w2(j+1,1:m)=w2(j,1:m)+b*k2;
  t2(j+1)=t2(j)+h2;
end
%
save data_3body_h2