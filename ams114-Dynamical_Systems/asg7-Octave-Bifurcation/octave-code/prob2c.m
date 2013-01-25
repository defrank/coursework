# prob2c.m
function prob2c
clear
figure(1);
hold off
[x,y] = meshgrid(-2:.2:2);
mu=2;
x_dot=-y+(mu.*x)+(x.*(y.^2));
y_dot=x+(mu.*y)-(x.^2);
h=quiver(x,y,x_dot,y_dot);
set(h, "autoscalefactor", 3);
axis("tight");
title("mu = 2");
xlabel("x");
ylabel("y");
fixAxes;
endfunction
