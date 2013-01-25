# lorenzeq.m
function lorenzeq
clear
figure(1);
hold off
[x,y,z] = meshgrid(-5:1:5);
sig=1;
r=1;
b=1;
x_dot=sig.*(y-x);
y_dot=(r.*x)-(y)-(x.*z);
z_dot=(x.*y)-(b.*z);
h=quiver(x,y,z,x_dot,y_dot,z_dot);
set(h, "autoscalefactor", 4);
axis("tight");
title("Lorenz Equation");
xlabel("x");
ylabel("y");
zlabel("z");
fixAxes;
endfunction
