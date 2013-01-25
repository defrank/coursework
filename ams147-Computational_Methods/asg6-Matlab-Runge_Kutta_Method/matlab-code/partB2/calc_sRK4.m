% This code implements the classical four stage fourth order
% Runge Kutta method to solve an ODE system. After the
% calculation, it saves the workspace to a data file.
%
clear
%
m=2;
w0=[1.5, 0];
%
h1=0.2; h2=0.2*2.^(-1); h3=0.2*2.^(-2); h4=0.2*2.^(-3);
h5=0.2*2.^(-4); h6=0.2*2.^(-5); h7=0.2*2.^(-6);
%
nstep1=25/h1; nstep2=25/h2; nstep3=25/h3; nstep4=25/h4;
nstep5=25/h5; nstep6=25/h6; nstep7=25/h7;
%
t1=zeros(nstep7+1,1); t2=zeros(nstep7+1,1); t3=zeros(nstep7+1,1);
t4=zeros(nstep7+1,1); t5=zeros(nstep7+1,1);
t6=zeros(nstep7+1,1); t7=zeros(nstep7+1,1);
t1(1)=0; t2(1)=0; t3(1)=0; t4(1)=0; t5(1)=0; t6(1)=0; t7(1)=0;
%
w1=zeros(nstep1+1,m); w2=zeros(nstep2+1,m); w3=zeros(nstep3+1,m);
w4=zeros(nstep4+1,m); w5=zeros(nstep5+1,m);
w6=zeros(nstep6+1,m); w7=zeros(nstep6+1,m);
w1(1,1:m)=w0; w2(1,1:m)=w0; w3(1,1:m)=w0; w4(1,1:m)=w0;
w5(1,1:m)=w0; w6(1,1:m)=w0; w7(1,1:m)=w0;
%
p=4;
d=[0,   1/2, 1/2, 1  ];
c=[0,   0,   0,   0  ;
   1/2, 0,   0,   0  ;
   0,   1/2, 0,   0  ;
   0,   0,   1,   0  ]; 
b=[1/6, 1/3, 1/3, 1/6];
k1=zeros(p,m); k2=zeros(p,m); k3=zeros(p,m);
k4=zeros(p,m); k5=zeros(p,m);
k6=zeros(p,m); k7=zeros(p,m);
% calculate RK4
for j=1:nstep1,
  for i=1:p,
    k1(i,1:m)=h1*f_sys(w1(j,1:m)+c(i,1:i-1)*k1(1:i-1,1:m), t1(j)+d(i)*h1);
  end
  w1(j+1,1:m)=w1(j,1:m)+b*k1;
  t1(j+1)=t1(j)+h1;
end
for j=1:nstep2,
  for i=1:p,
    k2(i,1:m)=h2*f_sys(w2(j,1:m)+c(i,1:i-1)*k2(1:i-1,1:m), t2(j)+d(i)*h2);
  end
  w2(j+1,1:m)=w2(j,1:m)+b*k2;
  t2(j+1)=t2(j)+h2;
end
for j=1:nstep3,
  for i=1:p,
    k3(i,1:m)=h3*f_sys(w3(j,1:m)+c(i,1:i-1)*k3(1:i-1,1:m), t3(j)+d(i)*h3);
  end
  w3(j+1,1:m)=w3(j,1:m)+b*k3;
  t3(j+1)=t3(j)+h3;
end
for j=1:nstep4,
  for i=1:p,
    k4(i,1:m)=h4*f_sys(w4(j,1:m)+c(i,1:i-1)*k4(1:i-1,1:m), t4(j)+d(i)*h4);
  end
  w4(j+1,1:m)=w4(j,1:m)+b*k4;
  t4(j+1)=t4(j)+h4;
end
for j=1:nstep5,
  for i=1:p,
    k5(i,1:m)=h5*f_sys(w5(j,1:m)+c(i,1:i-1)*k5(1:i-1,1:m), t5(j)+d(i)*h5);
  end
  w5(j+1,1:m)=w5(j,1:m)+b*k5;
  t5(j+1)=t5(j)+h5;
end
for j=1:nstep6,
  for i=1:p,
    k6(i,1:m)=h6*f_sys(w6(j,1:m)+c(i,1:i-1)*k6(1:i-1,1:m), t6(j)+d(i)*h6);
  end
  w6(j+1,1:m)=w6(j,1:m)+b*k6;
  t6(j+1)=t6(j)+h6;
end
for j=1:nstep7,
  for i=1:p,
    k7(i,1:m)=h7*f_sys(w7(j,1:m)+c(i,1:i-1)*k7(1:i-1,1:m), t7(j)+d(i)*h7);
  end
  w7(j+1,1:m)=w7(j,1:m)+b*k7;
  t7(j+1)=t7(j)+h7;
end

save data_sRK4