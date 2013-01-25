clear
%
load data_3body
td=t(:,1);
yd=w(:,1);
%
Ta=[5:0.04:8];
n=size(Ta,2);
da=zeros(1,n);
%
for k=1:n,
  da(k)=dd(Ta(k),td,yd);
end
%
save data_d_T