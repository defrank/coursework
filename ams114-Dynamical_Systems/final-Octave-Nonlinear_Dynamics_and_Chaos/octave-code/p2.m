# p2.m
function p2
clear
figure(1)
hold on
[x,y]=meshgrid(-2:.2:5);
x_dot=x.*(2-x-y);
y_dot=y.*((4.*x)-(x.^2)-3);
h=quiver(x,y,x_dot,y_dot);
set(h,"autoscalefactor",4);
axis("tight");
title("Phase Portrait");
xlabel("x");
ylabel("y");
fixAxes;
endfunction
