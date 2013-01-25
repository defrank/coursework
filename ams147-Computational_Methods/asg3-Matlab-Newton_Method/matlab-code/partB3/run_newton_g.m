%
% Consider the non-linear system
%   g1(u,v,c)=0
%   g2(u,v,c)=0
% This code calculates the root vector (u, v) for c = [5.8, 10]
%
clear
figure(3)
clf
axes('position',[0.15,0.13,0.75,0.75])
%
c=[5.8:.1:10];
N=42;
r_u=zeros(N);
r_v=zeros(N);
n=zeros(N);
tol=1.0e-10;
%
r_u(1)=0.1;
r_v(1)=-0.8;
[r_u(1), r_v(1), n(1)]=newton_sys('g', 'gp_2', c(1), r_u(1), r_v(1), tol);
for i=2:N,
    r_u(i)=r_u(i-1);
    r_v(i)=r_v(i-1);
    [r_u(i), r_v(i), n(i)]=newton_sys('g', 'gp_2', c(i), r_u(i), r_v(i), tol);
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
axis([5.7,10.1,-.9,.7])
set(gca,'xtick',[6:1:10])
set(gca,'ytick',[-.8:.2:.6])
set(gca, 'fontsize', 14)
xlabel('c')
ylabel('Root r(c)=(u,v), r=(g1(c), g2(c))')
title('The root vector as a function of c, r(c)=(u,v)')
legend('u=g1(c)','v=g2(c)')