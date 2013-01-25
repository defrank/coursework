clear
figure(4)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load data_d_T
%
plot(T,dd_T,'linewidth',2.0)
axis([5.9,9.1,-.5,14])
set(gca,'fontsize',14)
set(gca,'xtick',[6:9])
%set(gca,'ytick',[0:5:15])
xlabel('T')
ylabel('dd(T)')
title('Possible values of period T for y``+sin(y)=0, t=[0,20]')
for i=1:sizeT,
    disp(['        T = ',num2str(T(i),'%16.8e'),'.'])
    disp(['        dd(T) = ',num2str(dd_T(i),'%16.8e'),'.'])
    disp(' ')
end