clear
figure(8)
clf reset
axes('position',[0.15,0.13,0.75,0.75])
%
load data_3body_h2
%
diff=zeros(nstep+1,m);
for j=1:m,
    for i=0:nstep,
        diff(i+1,j)=abs(w(i+1,j)-w2(2*i+1,j));
    end
end
err=zeros(nstep+1);
for i=1:nstep+1,
    err(i)=(16/15)*norm(diff(i,1)+diff(i,2)+diff(i,3)...
        +diff(i,4)+diff(i,5)+diff(i,6)+diff(i,7)+diff(i,8)...
        +diff(i,9)+diff(i,10)+diff(i,11)+diff(i,12));
end
%
semilogy(t, err,'k-')
%hold on
%loglog(t, err,'bs','Markerfacecolor','b')
%
axis([-.2,10,1e-11,1e-6])
set(gca,'fontsize',14)
set(gca,'xtick',[0:2:10])
set(gca,'ytick',10.^[-12:1:-2])
xlabel('Time, t')
ylabel('Estimated error')
title('Fourth order method error, time step h = 1/512')