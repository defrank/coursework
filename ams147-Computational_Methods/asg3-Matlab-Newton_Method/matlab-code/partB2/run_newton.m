%
% Consider the non-linear system
%   exp(u) - cos(v) + u - v - c = 0
%   exp(v) + sin(u) + v + u = 0
% This code calculates the root vector (u, v) for c = 10
%
clear
figure(2)
clf
axes('position',[0.15,0.13,0.75,0.75])
%
c=[-10:1:30];
N=40;
r_u=zeros(N);
r_v=zeros(N);
n=zeros(N);
tol=1.0e-10;
%
for i=1:N,
    r_u(i)=1;
    r_v(i)=1;
    [r_u(i), r_v(i), n(i)]=newton_sys('f', 'fp_2', c(i), r_u(i), r_v(i), tol);
end
%
for j=1:N,
    r=[r_u(j) r_v(j)]';
    disp('  ')
    disp(['  The root found is (u, v) = (',num2str(r(1),'%16.8e'),', ',num2str(r(2),'%16.8e'),').'])
    disp(['  It takes n = ',num2str(n(j)),' iterations to reach err <= ',...
        num2str(tol),'.'])
    disp('  ')
    plot(c(j), r_u(j), 'sr', 'MarkerFaceColor', 'r')
    hold on
    plot(c(j), r_v(j), 'b-o')
end
%
axis([-12,31,-9,4])
set(gca,'xtick',[-10:8:30])
set(gca,'ytick',[-8:2:4])
set(gca, 'fontsize', 14)
xlabel('c')
ylabel('Root r(c)=(u,v), r=(f1(c),f2(c))')
title('The root vector as a function of c, r(c)=(u,v)')
legend('u=f1(c)','v=f2(c)')