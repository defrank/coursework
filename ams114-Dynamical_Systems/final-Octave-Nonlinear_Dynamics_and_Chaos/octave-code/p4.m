# p4.m
function p4
clear
clf
figure(1)
hold off
[x,y]=meshgrid(-4:.25:4);
x_dot=x.*(4-y-(x.^2));
y_dot=y.*(x-1);
h=quiver(x,y,x_dot,y_dot);
set(h,"autoscalefactor",3);
axis("tight");
title("Phase Portrait");
xlabel("x");
ylabel("y");
fixAxes;
endfunction
