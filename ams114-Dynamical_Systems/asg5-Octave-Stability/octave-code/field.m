# field.m
function field
clear
figure(1);
#clf("reset")
#
hold off
x=[-2:0.2:2];
y=[-2:0.2:2];
for i=1:length(x)
  for j=1:length(y)  
#    size(x(i));
    vopt=odeset('RelTol',1e-6,'AbsTol',1e-6,'InitialStep',.001,'MaxStep',.001,'OutputFcn',@odeplot);
    [t,s]=ode45(@f,[0 0.1],[x(i);y(j)],vopt);
 #   plot([s(1,1) s(length(t),1)],[s(1,2) s(length(t),2)],'b-');
    hold on
 #   plot(s(length(t),1),s(length(t),2),'ro');
 #   hold on
  endfor
endfor
#hold off
grid on;
axis equal
axis([-5,5,-5,5])
xlabel('x(t)'); ylabel('y(t)');
#axis([min(x),max(x),min(y),max(y)])
endfunction
