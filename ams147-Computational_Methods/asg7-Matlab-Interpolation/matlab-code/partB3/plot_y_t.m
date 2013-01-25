clear
figure(5)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load -ascii data7.txt
td=data7(:,1);
yd=data7(:,2);
%
plot(td,yd,'linewidth',2.0)
axis([0,20,-2.5,2.5])
set(gca,'fontsize',14)
set(gca,'xtick',[0:5:20])
set(gca,'ytick',[-2.0:1:2.0])
xlabel('t')
ylabel('y')