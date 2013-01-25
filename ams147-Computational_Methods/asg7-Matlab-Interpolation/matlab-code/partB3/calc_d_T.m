clear
%
load data_sRK4c
%
T=[6:.03:9];
sizeT=size(T,2);
dd_T=zeros(1,sizeT);
%
for k=1:sizeT,
  dd_T(k)=dd(T(k),t,wc(1:nstep+1,1));
end
%
save data_d_T