# prob2a.m
function prob2a
clear
figure(1);
hold off
[x,y] = meshgrid(-2:.2:2);
mu=-2;
x_dot=-y+(mu.*x)+(x.*(y.^2));
y_dot=x+(mu.*y)-(x.^2);
h=quiver(x,y,x_dot,y_dot);
set(h, "autoscalefactor", 4);
axis("tight");
title("mu = -2");
xlabel("x");
ylabel("y");
fixAxes;
endfunction
