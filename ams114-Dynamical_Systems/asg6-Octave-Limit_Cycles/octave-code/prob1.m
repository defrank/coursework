# prob1.m
function prob1
clear
figure(1);
hold off
[x,y] = meshgrid(-2:.2:2);
x_dot=y;
y_dot=-x+y.*(1-x.^2);
h=quiver(x,y,x_dot,y_dot);
set(h, "autoscalefactor", 5);
axis("tight");
title("van der Pol oscillator");
xlabel("x");
ylabel("y");
fixAxes;
endfunction
