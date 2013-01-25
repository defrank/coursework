% This code determines the order of accuracy for the
% int_a^b f(x)=exp(-sqrt(x)) dx
% using the composite Simpson rule.
% 
clear
figure(5)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
a=0.0; b=2.0;
Nsize=10;
N=2.^[1:1:Nsize];
h=(b-a)./N;
S1=zeros(Nsize); S2=zeros(Nsize); S4=zeros(Nsize);
for i=1:Nsize,
    [S1(i)]=simp_num_est(h(i),N(i));
    [S2(i)]=simp_num_est(h(i)/2,N(i));
    [S4(i)]=simp_num_est(h(i)/4,N(i));
end
err1=abs(S1(1:Nsize)-S2(1:Nsize))+1.0e-16;
err2=abs(S2(1:Nsize)-S4(1:Nsize))+1.0e-16;
p=log2((err1(1:Nsize))./(err2(1:Nsize)));
%
semilogx(N(1:Nsize),p,'bo')
set(gca,'fontsize',14)
disp(['p = [',num2str(p),']'])
for i=1:Nsize,
    disp(['S1 = [',num2str(S1(i)),']'])
    disp(['S2 = [',num2str(S2(i)),']'])
    disp(['S4 = [',num2str(S4(i)),']'])
end