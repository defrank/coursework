# prob3.m
function prob3
clear
figure(1);
hold off
[x,y] = meshgrid(-1:.1:1);
mu=0;
x_dot=y+(mu.*x);
y_dot=-x+(mu.*y)-((x.^2).*y);
h=quiver(x,y,x_dot,y_dot);
set(h, "autoscalefactor", 5);
axis("tight");
title("mu = 0");
xlabel("x");
ylabel("y");
fixAxes;
endfunction
