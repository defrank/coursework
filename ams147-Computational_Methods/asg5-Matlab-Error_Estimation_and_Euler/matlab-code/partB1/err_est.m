% This code estimates the error of the numerical integration
% method and plots it as a function of the number of steps, N,
% per spatial step, h.
%
clear
figure(1)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
a=0.0; b=2.0;
N=2.^([1:1:10]);
Nsize=10;
h=(b-a)./N;
%
T=zeros(Nsize); S=zeros(Nsize);
for i=1:Nsize,
    [T(i)]=trap_num_est(h(i),N(i));
    [S(i)]=simp_num_est(h(i),N(i));
end
% second order
errT=abs(T(1:Nsize-1)-T(2:Nsize))/(1-0.5^2)+1.0e-16;
% fourth order
errS=abs(S(1:Nsize-1)-S(2:Nsize))/(1-0.5^4)+1.0e-16;
%
loglog(N(1:Nsize-1), errT,'bo', 'markerfacecolor', 'b')
hold on
loglog(N(1:Nsize-1), errS,'rs', 'markerfacecolor', 'r')
hold on
%
%axis([10^(-1/4),10^(10/3),10^(-1/4),10^(3/4)])
set(gca,'fontsize',14)
%set(gca,'xtick',10.^[0:1:3])
%set(gca,'ytick',10.^[0:1/4:1])
xlabel('Number of steps, N')
ylabel('Estimated error')
title('Estimated Error for the composite Trapezoidal and Simpson rule')
legend('Trapezoidal', 'Simpson')