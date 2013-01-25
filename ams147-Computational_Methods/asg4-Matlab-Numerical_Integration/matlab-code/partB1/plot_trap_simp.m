% Consider the integral from [a,b] of sinxdx.
% Two curves are approximated using first composite Trapezoidal rule
% and second with the composite Simpson rule.  Each is evaluated with
% a step size of h=(b-a)/N, where N=2^[1:1:10]. The total error is
% then solved for with the exact value = cosa-cosb.
% The code "trapezoidal.m" approximates total error for all N and
% stores the data in "trap.mat." The code "simpson.m" approximates
% total error for all N and stores the data in "simp.mat."
% This code loads in "trap.mat" and "simp.mat" and plots the errors as
% a function of N.
%
clear
figure(1)
clf
axes('position',[0.15,0.13,0.75,0.75])
%
load trap.mat T errT N M
load simp.mat S errS
% plot curve
for i=1:M,
    loglog(N(i), errT(i),'rs', 'markerfacecolor', 'r')
    hold on
    loglog(N(i), errS(i),'bo', 'markerfacecolor', 'b')
    hold on
end
set(gca,'fontsize',14)
xlabel('N = 2^[^1^:^1^:^1^0^]')
ylabel('Total Error, E(N)=err(N)')
title('Total Error as a function of N, E(N), for Integration Estimation')
legend('Trapezoidal error, err_T(N)', 'Simpson error, err_S(N)')