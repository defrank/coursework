clear
%
load data_sRK4c
%
T=[6:3/100:9];
sizeT=101;
dd_T=zeros(1,sizeT);
%
for k=1:sizeT,
  dd_T(k)=dd(T(k),t,wc(1:nstep+1,1));
end
%
save data_d_T