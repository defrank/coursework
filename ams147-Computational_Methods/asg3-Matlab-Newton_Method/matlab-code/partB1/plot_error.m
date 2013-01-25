% The first, second, and fourth order errors are calculated
% and plotted as a function of h.

clear
figure(1)
clf
axes('position',[0.15,0.13,0.75,0.75])

% h is between [10.^-13, 10.^-1]
h=10.^([-13:.1:-1]);
% This function calculates the first order error error as a
% function of h.
e1=h+(1*(10.^-16))./h;
% This function calculates the second order error error as a
% function of h.
e2=(h.^2)+(1*(10.^-16))./h;
% This function calculates the fourth order error error as a
% function of h.
e4=(h.^4)+(1*(10.^-16))./h;

loglog(h, e1, 'gs', 'markerfacecolor', [.5,.5,.5])
hold on
loglog(h, e2, 'b-o')
hold on
loglog(h, e4, 'k-', 'linewidth',2.0)
set(gca,'fontsize',14)
xlabel('h')
ylabel('Error, e(h)')
title('First, Second, and Fourth Order Error Functions')
legend('First Order', 'Second Order', 'Fourth Order')