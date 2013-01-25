# prob2b.m
function prob2b
clear
figure(1);
hold off
[x,y] = meshgrid(-2:.2:2);
mu=0;
x_dot=-y+(mu.*x)+(x.*(y.^2));
y_dot=x+(mu.*y)-(x.^2);
h=quiver(x,y,x_dot,y_dot);
set(h, "autoscalefactor", 5);
axis("tight");
title("mu = 0");
xlabel("x");
ylabel("y");
fixAxes;
endfunction
