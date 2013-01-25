% This code determines the order of accuracy for the
% int_a^b f(x)=exp(-sqrt(x)) dx
% using the composite Simpson rule.
% 
clear
figure(2)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
a=0.0; b=2.0;
Nsize=10;
N=2.^[1:1:Nsize];
h=(b-a)./N;
S=zeros(Nsize);
for i=1:Nsize,
    [S(i)]=simp_num_est(h(i),N(i));
end
%
err1=abs(S(1:Nsize-1)-S(2:Nsize))+1.0e-16;
err2=abs(S(2:Nsize-1)-S(3:Nsize))+1.0e-16;
p=log2((err1(1:Nsize-2))./(err2(1:Nsize-2)));
%
semilogx(N(1:Nsize-2),p,'bo','markerfacecolor','b')
set(gca,'fontsize',14)
xlabel('Number of steps, N')
ylabel('Order of accuracy')
title('Estimated Order of Accuracy for composite Simpson rule')
legend('Order of a method, p(N)')
%
disp(['p = [',num2str(p),']'])
for i=1:Nsize,
    disp(['S = [',num2str(S(i)),']'])
end