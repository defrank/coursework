# prob4.m
function prob4
clear
figure(1);
hold off
[x,y]=meshgrid(-2:.1:2);
mu=5;
x_dot=y;
y_dot=((1-(exp(2.*x)))./(1+(exp(2.*x))))-mu.*((x.^2)-1).*y;
h=quiver(x,y,x_dot,y_dot);
set(h,"autoscalefactor",2);
axis("tight");
title("mu = 5");
xlabel("x");
ylabel("y");
fixAxes;
endfunction
